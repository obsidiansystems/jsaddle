{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Run
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Run (
  -- * Running JSM
#ifndef ghcjs_HOST_OS
  -- * Functions used to implement JSaddle using JSON messaging
    runJavaScript
  , newJson
  , sync
  , lazyValResult
  , freeSyncCallback
  , newSyncCallback'
  , newSyncCallback''
  , callbackToSyncFunction
  , callbackToAsyncFunction
  , syncPoint
  , getProperty
  , setProperty
  , getJson
  , getJsonLazy
  , callAsFunction'
  , callAsConstructor'
#endif
) where

#ifndef ghcjs_HOST_OS
import Control.Exception (try, SomeException(..), throwIO)
import Control.Monad (when, join, void, unless, forever)
import Control.Monad.Except (catchError)
import Control.Monad.Trans.Reader (runReaderT, asks)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (STM, atomically, retry)
import Control.Concurrent (myThreadId, forkIO, threadDelay, forkOS)
import Control.Concurrent.Async (race_, race)
import Control.Concurrent.STM.TVar (writeTVar, readTVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
       (putMVar, takeMVar, newMVar, newEmptyMVar, modifyMVar, modifyMVar_, swapMVar, tryPutMVar, MVar, tryReadMVar)

import Control.DeepSeq
import Control.Exception

import Data.Monoid ((<>))
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHCJS.Prim.Internal (primToJSVal)

import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Value (valToText)
--TODO: Handle JS exceptions
import Data.Foldable (forM_, traverse_, foldl')
import Language.Javascript.JSaddle.Monad (syncPoint)

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Functor

-- | The first dynamically-allocated RefId
initialRefId :: RefId
initialRefId = RefId 2

type CallbackResult = Either SomeException (Either JavaScriptException JSVal)

runJavaScript
  :: ([TryReq] -> IO ()) -- ^ Send a batch of requests to the JS engine; we assume that requests are performed in the order they are sent; requests received while in a synchronous block must not be processed until the synchronous block ends (i.e. until the JS side receives the final value yielded back from the synchronous block)
  -> IO ( [Rsp] -> IO () -- Responses must be able to continue coming in as a sync block runs, or else the caller must be careful to ensure that sync blocks are only run after all outstanding responses have been processed
        , SyncCommand -> IO [(Int, SyncBlockReq)]
        , JSContextRef
        , JSVal
        )
  -- These default have been determined to give good results on jsaddle-warp
  -- Tested with jsaddle-benchmark (https://github.com/obsidiansystems/jsaddle-benchmark)
runJavaScript = runJavaScriptInt (500 {- 0.5 ms -}) 100

data TuningParams = TuningParams
  { _tuningParams_timeout :: Int -- Maximum microseconds to delay outgoing async requests for batching purposes
  , _tuningParams_sufficientBatchSize :: Int -- Maximum number of async messages to wait for before sending a batch; note that the batch may end up being bigger, but there will not be any intentional delay after the batch size is reached
  }

data RequestMode
   = RequestMode_Async
   | RequestMode_Sync
   deriving (Eq, Ord, Show, Read)

batchReqs
  :: forall req
  .  TuningParams
  -> ([req] -> IO ()) -- Send a batch of requests; this must not throw exceptions, or batchReqs will fail
  -> IO (Seq req -> STM (), IO ())
batchReqs tuningParams sendReqBatchAsync = do
  sendImmediately <- newTVarIO False
  pendingReqs <- newTVarIO (mempty :: Seq req)
  batchLargeEnough <- newEmptyMVar
  _ <- forkIO $ forever $ do
    -- Wait for at least one thing to be pending
    atomically $ do
      rs <- readTVar pendingReqs
      when (Seq.null rs) retry
    -- Wait for either the timeout to elapse, the batch to get large enough, or an explicit signal to send.  Note that we do not atomically capture batches that have hit the max batch size, so the batch we ultimately send may *exceed* the batch size.
    let waitForTimeout = threadDelay $ _tuningParams_timeout tuningParams
        waitForBatchReady = atomically $ do
          si <- readTVar sendImmediately
          when (not si) $ do
            rs <- readTVar pendingReqs
            when (Seq.length rs < _tuningParams_sufficientBatchSize tuningParams) retry
    race_ waitForTimeout waitForBatchReady
    reqsToSend <- atomically $ do
      toSend <- readTVar pendingReqs
      writeTVar pendingReqs mempty
      writeTVar sendImmediately False -- Only applies to the batch in progress when it is set
      pure toSend
    sendReqBatchAsync $ toList reqsToSend
  let enqueueReqs reqs = modifyTVar' pendingReqs (<> reqs)
  pure (enqueueReqs, atomically $ writeTVar sendImmediately True)

requestManager
  :: forall req
  .  TuningParams
  -> ([req] -> IO ()) -- Send a block of messages asynchronously to the JS side
  -> IO ( req -> IO () -- Make a request to JS from HS.  Whether this is synchronous or not will be automatically determined.  "Synchronous" in this context means that the JS engine is blocked, so it only makes sense for messages from JS to HS to enter us into the synchronous mode.  If HS to JS messages want to be treated as synchronous, they should request from the JS side and then use an MVar or similar to block waiting for the result.
        , IO () -- Flush any pending requests
        , Int -> IO () -- Acknowledge that all reqs up to the given number have been processed; this counts since the beginning of the stream. This is idempotent. --TODO: 32 bits is probably not enough for this, but that's what JS will give us.  We need a better approach.  Note that just sending incremental "we finished processing this many" notifications doesn't work, because when we switch to sync mode we need to know how many to throw away, and we may not have yet received asynchronous acknowledgements.
        , IO [req] -- Initiate synchronous mode; get all the requests that have been sent asynchronously but not acknowledged; they are retransmitted here, and should be ignored when they are eventually received asynchronously.
        , IO () -- Terminate synchronous mode
        , IO [req] -- Wait for at least one request to be ready, the return it.  Must be in synchronous mode.
        )
requestManager tuningParams sendReqBatchAsync = do
  mode <- newTVarIO RequestMode_Async
  (enqueueAsyncReqs, sendImmediately) <- batchReqs tuningParams sendReqBatchAsync
  ackedReqs <- newTVarIO 0
  asyncSentReqs <- newTVarIO (mempty :: Seq req)
  pendingSyncReqs <- newTVarIO (mempty :: Seq req)
  let enqueueReq req = atomically $ do
        readTVar mode >>= \case
          RequestMode_Async -> do
            enqueueAsyncReqs $ Seq.singleton req
            modifyTVar' asyncSentReqs (|> req)
          RequestMode_Sync -> do
            modifyTVar' pendingSyncReqs (|> req)
      ackReqs newAcked = atomically $ do
        oldAcked <- readTVar ackedReqs
        when (newAcked > oldAcked) $ do
          writeTVar ackedReqs newAcked
          old <- readTVar asyncSentReqs
          let !new = Seq.drop (newAcked - oldAcked) old
          writeTVar asyncSentReqs new
      startSync = do
        (oldMode, reqsToResend) <- atomically $ do
          oldMode <- readTVar mode
          writeTVar mode RequestMode_Sync
          reqsToResend <- readTVar asyncSentReqs
          writeTVar asyncSentReqs mempty
          pure (oldMode, reqsToResend)
        when (oldMode == RequestMode_Sync) $ putStrLn $ "warning: requestManager: entered sync mode when we were already in sync mode"
        pure $ toList reqsToResend
      endSync = do
        oldMode <- atomically $ do
          oldMode <- readTVar mode
          writeTVar mode RequestMode_Async
          reqs <- readTVar pendingSyncReqs
          writeTVar asyncSentReqs reqs
          enqueueAsyncReqs reqs
          pure oldMode
        when (oldMode == RequestMode_Async) $ putStrLn $ "warning: requestManager: entered async mode when we were already in async mode"
      dequeueAllPendingReqs = fmap toList $ atomically $ do
        old <- readTVar pendingSyncReqs
        if Seq.null old then retry else do
          writeTVar pendingSyncReqs mempty
          pure old
  pure (enqueueReq, sendImmediately, ackReqs, startSync, endSync, dequeueAllPendingReqs)

runJavaScriptInt
  :: Int
  -- ^ Timeout for sending async requests in microseconds
  -> Int
  -- ^ Max size of async requests batch size
  -> ([TryReq] -> IO ())
  -- ^ See comments for runJavaScript
  -> IO ( [Rsp] -> IO ()
        , SyncCommand -> IO [(Int, SyncBlockReq)]
        , JSContextRef
        , JSVal
        )
runJavaScriptInt sendReqsTimeout pendingReqsLimit sendReqsBatch = do
  logQueue <- newChan
  forkIO $ forever $ do
    logLine <- readChan logQueue
    T.putStrLn logLine
  let log = writeChan logQueue
  {-}
  let log _ = pure ()
  --}
  let sendAsyncReqsBatch b = do
        let b' = b <&> \case
              (0, SyncBlockReq_Req r) -> r
              sr -> error $ "Trying to send a req async, but it needs to be sent inside a sync block: " <> show sr
        log ("sendReqsBatch " <> tshow b')
        sendReqsBatch b'
  (enqueueReq, sendImmediately, ackReqs, startSync, endSync, dequeueAllPendingReqs) <- requestManager @(Int, SyncBlockReq) (TuningParams sendReqsTimeout pendingReqsLimit) sendAsyncReqsBatch
  --TODO: Call endSync sometimes
  nextRefId <- newTVarIO initialRefId
  nextGetJsonReqId <- newTVarIO $ GetJsonReqId 1
  getJsonReqs <- newTVarIO M.empty
  nextCallbackId <- newTVarIO $ CallbackId 1
  callbacks <- newTVarIO M.empty
  nextTryId <- newTVarIO $ TryId 1
  tries <- newTVarIO M.empty
  pendingResults <- newTVarIO M.empty
  -- Each value in the map corresponds to a value ready to be returned from the sync frame corresponding to its key
  -- INVARIANT: \(depth, readyFrames) -> all (< depth) $ M.keys readyFrames
  syncCallbackState <- newMVar (0, M.empty, M.empty)
  syncState <- newMVar SyncState_InSync
  nextSyncReqId <- newTVarIO $ SyncReqId 1
  syncReqs <- newTVarIO mempty
  threadId <- myThreadId
  let {-
      tryEnterSyncFrame :: (Int -> MVar TryId -> IO CallbackResult) -> IO [(Int, SyncBlockReq)]
      tryEnterSyncFrame startNewFrame = modifyMVar syncCallbackState $ \(oldDepth, readyFrames, oldFrameTries) -> modifyMVar yieldAccumVar $ \(resultReady, old) -> do
        let
          isThrow req = case req of
            SyncBlockReq_Throw _ _ -> True
            _ -> False
          -- If we have a throw on a lower frame, then the new frame should not be started
          -- Need to do throw immediately on the new frame
          startingNewFrame = not $ any isThrow $ M.elems readyFrames
          !newResultReady = if startingNewFrame then False else resultReady
          -- these are sent immediately
          new
            | not startingNewFrame =
              (succ oldDepth, SyncBlockReq_Throw (succ oldDepth) (Left "AsyncCancelled: Lower frame has exception")) : (reverse old)
            | otherwise = reverse old
          !newDepth = if startingNewFrame then succ oldDepth else oldDepth
        newFrameTries <- if startingNewFrame
          then do
            tryMVar <- newEmptyMVar
            void $ forkIO $ (exitSyncFrame newDepth =<< startNewFrame newDepth tryMVar)
            (\t -> M.insertWith (error "frame's tryId already present") newDepth t oldFrameTries)
              <$> takeMVar tryMVar
          else pure oldFrameTries
        unless (newResultReady || (null old && not resultReady)) $ do
          log $ "tryEnterSyncFrame: taking yieldReadyVar"
          takeMVar yieldReadyVar
        return ((newResultReady, []), ((newDepth, readyFrames, newFrameTries), new))
      exitSyncFrame :: Int -> CallbackResult -> IO ()
      exitSyncFrame myDepth myRetVal = modifyMVar_ syncCallbackState $ \(oldDepth, oldReadyFrames, oldFrameTries) -> case oldDepth `compare` myDepth of
        LT -> error "should be impossible: trying to return from deeper sync frame than the current depth"
        -- Just store our value so it can be yielded later
        _ -> do
          !syncBlockReq <- case myRetVal of
            Left e -> pure $ SyncBlockReq_Throw myDepth (Left $ T.pack $ show e)
            -- Even though the valId is escaping, this is safe because we know that our yielded value will
            -- go out before any potential FreeVal request could go out
            -- The FreeVal request using this 'env' will be done async after all sync frames.
            Right v -> flip runReaderT env $ unJSM $ withJSValId (either unJavaScriptException id v) $ \retValId -> do
              pure $ case v of
                Left _ -> SyncBlockReq_Throw myDepth (Right retValId)
                Right _ -> SyncBlockReq_Result retValId
          let !newReadyFrames = M.insertWith (error "should be impossible: trying to return from a sync frame that has already returned") myDepth syncBlockReq oldReadyFrames
          !newFrameTries <- case myRetVal of
            Right _ -> pure (M.delete myDepth oldFrameTries)
            Left _ -> do
              let
                (!newFrameTries, toStop) = M.split myDepth oldFrameTries
                stopTry tryId = do
                  mTryMVar <- atomically $ do
                    currentTries <- readTVar tries
                    writeTVar tries $! M.delete tryId currentTries
                    return $ M.lookup tryId currentTries
                  forM_ mTryMVar $ \v ->
                    putMVar v $ Left $ primToJSVal $ PrimVal_String "Parent Try received an exception."
              mapM_ stopTry (M.elems toStop)
              pure newFrameTries
          when (myDepth == oldDepth) $ modifyMVar_ yieldAccumVar $ \(resultReady, old) -> do
            when ((null old) && (not resultReady)) $ do
              log $ "exitSyncFrame: putting yieldReadyVar"
              putMVar yieldReadyVar ()
            return (True, old)
          return (oldDepth, newReadyFrames, newFrameTries)

      yield = modifyMVar syncCallbackState $ \(oldDepth, oldReadyFrames, oldFrameTries) -> do
        let yieldAllReady :: (Int, Map Int SyncBlockReq)
              -> ([(Int, SyncBlockReq)], (Int, Map Int SyncBlockReq))
            yieldAllReady (depth, readyFrames) = case M.lookup depth readyFrames of
              Nothing -> ([], (depth, readyFrames))
              Just v -> ((depth,v):vs, remaining)
                where
                  (vs, remaining) = yieldAllReady (pred depth, M.delete depth readyFrames)
            (allResults, (newDepth, newReadyFrames)) = yieldAllReady (oldDepth, oldReadyFrames)
        requests <- reverse . snd <$> swapMVar yieldAccumVar (False, [])
        pure $ ((newDepth, newReadyFrames, oldFrameTries), allResults ++ requests)
-}
      processRsp = traverse_ $ \case
        Rsp_GetJson getJsonReqId val -> do
          reqs <- atomically $ do
            reqs <- readTVar getJsonReqs
            writeTVar getJsonReqs $! M.delete getJsonReqId reqs
            return reqs
          forM_ (M.lookup getJsonReqId reqs) $ \resultVar -> do
            putMVar resultVar val
        Rsp_Result refId primVal -> do
          mResultVar <- atomically $ do
            resultVars <- readTVar pendingResults
            let mResultVar = M.lookup refId resultVars
            when (isJust mResultVar) $ do
              writeTVar pendingResults $! M.delete refId resultVars
            return mResultVar
          forM_ mResultVar $ \resultVar -> do
            putMVar resultVar primVal
        Rsp_CallAsync callbackId this args -> do
          mCallback <- fmap (M.lookup callbackId) $ atomically $ readTVar callbacks
          case mCallback of
            Just callback -> do
              _ <- forkIO $ void $ flip runJSM env $ do
                _ <- join $ callback <$> wrapJSVal this <*> traverse wrapJSVal args
                return ()
              return ()
            Nothing -> error $ "callback " <> show callbackId <> " called, but does not exist"
        Rsp_FreeCallback callbackId -> do
          liftIO $ atomically $ modifyTVar' callbacks $ M.delete callbackId
        --TODO: We will need a synchronous version of this anyway, so maybe we should just do it that way
        Rsp_FinishTry tryId tryResult -> do
          mThisTry <- atomically $ do
            currentTries <- readTVar tries
            writeTVar tries $! M.delete tryId currentTries
            return $ M.lookup tryId currentTries
          case mThisTry of
            Nothing -> putStrLn $ "Rsp_FinishTry: " <> show tryId <> " not found"
            Just thisTry -> putMVar thisTry =<< case tryResult of
              Left v -> Left <$> runReaderT (unJSM (wrapJSVal v)) env
              Right _ -> return $ Right ()
        Rsp_Sync syncReqId -> do
          mThisSync <- atomically $ do
            currentSyncReqs <- readTVar syncReqs
            writeTVar syncReqs $! M.delete syncReqId currentSyncReqs
            return $ M.lookup syncReqId currentSyncReqs
          case mThisSync of
            Nothing -> putStrLn $ "Rsp_Sync: " <> show syncReqId <> " not found"
            Just thisSync -> putMVar thisSync ()
      env = JSContextRef
        { _jsContextRef_sendReq = \req -> do
            log $ "sendReq: " <> tshow req
            enqueueReq (0, SyncBlockReq_Req req)
        , _jsContextRef_notifyBlocking = sendImmediately
        , _jsContextRef_syncThreadId = Nothing
        , _jsContextRef_myThreadId = threadId
        , _jsContextRef_nextRefId = nextRefId
        , _jsContextRef_nextGetJsonReqId = nextGetJsonReqId
        , _jsContextRef_getJsonReqs = getJsonReqs
        , _jsContextRef_nextCallbackId = nextCallbackId
        , _jsContextRef_callbacks = callbacks
        , _jsContextRef_pendingResults = pendingResults
        , _jsContextRef_nextTryId = nextTryId
        , _jsContextRef_tries = tries
        , _jsContextRef_myTryId = TryId 0 --TODO
        , _jsContextRef_syncState = syncState
        , _jsContextRef_nextSyncReqId = nextSyncReqId
        , _jsContextRef_syncReqs = syncReqs
        , _jsContextRef_waitForResults = Nothing
        }
      processSyncCommand = \case
        SyncCommand_StartCallback acked (JSThreadId jsThreadId) callbackId this args -> do --TODO: Leave JSThreadId packed
          mCallback <- fmap (M.lookup callbackId) $ atomically $ readTVar callbacks
          case mCallback of
            Just (callback :: JSVal -> [JSVal] -> JSM JSVal) -> do
              log $ "processSyncCommand: Found callback " <> tshow callbackId
              threadId <- myThreadId
              syncStateLocal <- newMVar SyncState_InSync
              let syncEnv = env { _jsContextRef_sendReq = \req -> do
                                    -- We MUST fully evaluate our req here, because if we enqueue it while it is not fully evaluated, it could have thunks inside that block on lazy JSVals.  Since we batch requests, the JSVals it's blocked on might be part of the same batch.  This will result in a lockup, since we won't be able to send the batch until we receive responses which can't be sent until after the batch has been sent.
                                    evaluate $ rnf req
                                    log $ "syncEnv sendReq: " <> tshow req
                                    enqueueReq (jsThreadId, SyncBlockReq_Req req)
                                , _jsContextRef_syncThreadId = Just threadId
                                , _jsContextRef_myThreadId = threadId
                                , _jsContextRef_syncState = syncStateLocal }
                  run = do
                    (Right <$>) $ join $ callback <$> wrapJSVal this <*> traverse wrapJSVal args
              log $ "processSyncCommand: Running callback " <> tshow callbackId
              ackReqs acked
              result <- startSync --TODO: If this is empty, we ought to dequeueAllPendingReqs; however, if we did that, we would need to communicate that fact, so that the JS side knows it doesn't need to ignore any async Reqs
              forkIO $ flip runReaderT syncEnv $ unJSM $ do
                result <- run `catchError` (\e -> do
                  exceptionStr <- T.unpack <$> valToText (unJavaScriptException e)
                  unsafeInlineLiftIO $ putStrLn ("JavaScriptException happened in sync callback : " <> exceptionStr) >> pure (Left e))
                case result of
                  Left e ->
                    JSM $ liftIO $ enqueueReq (jsThreadId, SyncBlockReq_Throw $ Left $ tshow e) --TODO: Pass exception properly
                  Right r -> withJSValId r $ \rId ->
                    JSM $ liftIO $ enqueueReq (jsThreadId, SyncBlockReq_Result rId)
              pure result
            Nothing -> error $ "sync callback " <> show callbackId <> " called, but does not exist"
        SyncCommand_Continue -> dequeueAllPendingReqs
  arg <- flip runJSMCheap env $ do --Note: This must be runJSMCheap, because we cannot wait for a sync here
    argRef <- wrapRef $ RefId (-1)
    JSVal <$> lazyValResult argRef
  return
    ( \rsp -> do
        log $ "processRsp: " <> tshow rsp
        processRsp rsp
    , \syncCmd -> do
        log $ "processSyncCommand: " <> tshow syncCmd
        processSyncCommand syncCmd
    , env
    , arg
    )

#endif

tshow :: Show a => a -> Text
tshow = T.pack . show
