{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , globalRef
#endif
) where

#ifndef ghcjs_HOST_OS
import Control.Exception (try, SomeException(..), throwIO)
import Control.Monad.Except (catchError)
import Control.Monad (when, join, void, unless, forever)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (atomically)
import Control.Concurrent (myThreadId, forkIO, threadDelay, ThreadId, killThread)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TVar (writeTVar, readTVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Concurrent.MVar
       (putMVar, takeMVar, newMVar, newEmptyMVar, modifyMVar, modifyMVar_, swapMVar, tryPutMVar)

import Data.Foldable
import Data.Monoid ((<>))
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.IORef (newIORef)
import qualified Data.Text as T

import Language.Javascript.JSaddle.Types
--TODO: Handle JS exceptions
import Data.Foldable (forM_, traverse_)
import System.IO.Unsafe
import Language.Javascript.JSaddle.Monad (syncPoint)

-- | The RefId of the global object
globalRefId :: RefId
globalRefId = RefId 1

globalRef :: Ref
globalRef = unsafePerformIO $ Ref <$> newIORef globalRefId
{-# NOINLINE globalRef #-}

-- | The first dynamically-allocated RefId
initialRefId :: RefId
initialRefId = RefId 2

printSyncBlockReqs :: String ->  [(SyncFrameDepth, SyncBlockReq)] -> IO ()
printSyncBlockReqs l reqs = do
  putStrLn $ l <> " = ["
  for_ reqs $ \(d, r) -> do
    let rr = case r of
          SyncBlockReq_Req _ -> "Req"
          SyncBlockReq_Result _ -> "Result"
          SyncBlockReq_Throw t _ -> "Throw " <> (show t)
    putStrLn $ "  (" <> show d <> ", " <> rr <> ")"
  putStrLn "]"

runJavaScript
  :: ([TryReq] -> IO ()) -- ^ Send a batch of requests to the JS engine; we assume that requests are performed in the order they are sent; requests received while in a synchronous block must not be processed until the synchronous block ends (i.e. until the JS side receives the final value yielded back from the synchronous block)
  -> IO ( [Rsp] -> IO () -- Responses must be able to continue coming in as a sync block runs, or else the caller must be careful to ensure that sync blocks are only run after all outstanding responses have been processed
        , SyncCommand -> IO [SyncBlockReq]
        , JSContextRef
        )
runJavaScript = runJavaScriptInt (500 {- 0.5 ms -}) 100

runJavaScriptInt
  :: Int
  -- ^ Timeout for sending async requests in microseconds
  -> Int
  -- ^ Max size of async requests batch size
  -> ([TryReq] -> IO ())
  -- ^ See comments for runJavaScript
  -> IO ( [Rsp] -> IO ()
        , SyncCommand -> IO [SyncBlockReq]
        , JSContextRef
        )
runJavaScriptInt sendReqsTimeout pendingReqsLimit sendReqsBatch = do
  nextRefId <- newTVarIO initialRefId
  nextGetJsonReqId <- newTVarIO $ GetJsonReqId 1
  getJsonReqs <- newTVarIO M.empty
  nextCallbackId <- newTVarIO $ CallbackId 1
  callbacks <- newTVarIO M.empty
  nextTryId <- newTVarIO $ TryId 1
  tries <- newTVarIO M.empty
  pendingResults <- newTVarIO M.empty
  yieldAccumVar <- newMVar [] -- Accumulates results that need to be yielded
  yieldReadyVar <- newEmptyMVar -- Filled when there is at least one item in yieldAccumVar
  -- Each value in the map corresponds to a value ready to be returned from the sync frame corresponding to its key
  -- INVARIANT: \(depth, readyFrames) -> all (< depth) $ M.keys readyFrames
  syncCallbackState <- newMVar (SyncFrameDepth 0, M.empty, M.empty)
  syncState <- newMVar SyncState_InSync
  nextSyncReqId <- newTVarIO $ SyncReqId 1
  syncReqs <- newTVarIO mempty
  sendReqsBatchVar <- newMVar ()
  pendingReqs <- newTVarIO []
  pendingReqsCount <- newTVarIO (0 :: Int)
  let enqueueYieldVal val = do
        wasEmpty <- modifyMVar yieldAccumVar $ \old -> do
          let !new = val : old
          return (new, null old)
        when wasEmpty $ putMVar yieldReadyVar ()
      -- Update the syncCallbackState with threadId in modifyMVar to avoid race
      inSyncFrame :: (SyncFrameDepth -> ThreadId -> IO (Either SomeException CallbackResult)) -> IO ()
      inSyncFrame f = modifyMVar_ syncCallbackState $ \(oldDepth, readyFrames, runningThreads) -> do
        let !newDepth = succ oldDepth
        threadId <- forkIO $
          exitSyncFrame newDepth =<< f newDepth =<< myThreadId
        let !newRunningThreads = M.insertWith (error "should be impossible: thread already present")
              newDepth threadId runningThreads
        return (newDepth, readyFrames, newRunningThreads)
      exitSyncFrame :: SyncFrameDepth -> (Either SomeException CallbackResult) ->  IO ()
      exitSyncFrame myDepth callbackResult = modifyMVar_ syncCallbackState $ \(oldDepth, oldReadyFrames, oldRunningThreads) -> case oldDepth `compare` myDepth of
        LT -> do
          -- error "should be impossible: trying to return from deeper sync frame than the current depth"
          -- This can happen
          putStrLn $ "LT: " <> (show myDepth)
          pure (oldDepth, oldReadyFrames, oldRunningThreads)
        -- We're the top frame, so yield our value to the caller
        EQ -> case callbackResult of
          Right myRetVal -> do
            putStrLn $ "EQ: Right " <> (show myDepth)
            let yieldAllReady :: SyncFrameDepth -> Map SyncFrameDepth CallbackResult
                  -> Map SyncFrameDepth ThreadId -> CallbackResult
                  -> IO (SyncFrameDepth, Map SyncFrameDepth CallbackResult, Map SyncFrameDepth ThreadId)
                yieldAllReady depth readyFrames runningThreads retVal = do
                  -- Even though the valId is escaping, this is safe because we know that our yielded value will go out before any potential FreeVal request could go out
                  flip runReaderT env $ unJSM $ withJSValId retVal $ \retValId -> do
                    JSM $ liftIO $ enqueueYieldVal (depth, SyncBlockReq_Result retValId)
                  let !nextDepth = pred depth
                      !nextRunningThreads = M.delete depth runningThreads
                  case M.maxViewWithKey readyFrames of
                    -- The parent frame is also ready to yield
                    Just ((k, nextRetVal), nextReadyFrames)
                      | k == nextDepth
                        -> yieldAllReady nextDepth nextReadyFrames nextRunningThreads nextRetVal
                    _ -> return (nextDepth, readyFrames, nextRunningThreads)
            yieldAllReady oldDepth oldReadyFrames oldRunningThreads myRetVal
          Left someException -> do
            putStrLn $ "EQ: Left " <> (show myDepth)
            let !newRunningThreads = M.delete myDepth oldRunningThreads
            let !newReadyFrames = M.delete myDepth oldReadyFrames
            wasEmpty <- modifyMVar yieldAccumVar $ \old -> do
              -- Drop all pending requests of current frames.
              -- And throw
              let !new = (filter ((< pred myDepth) . fst) old) ++
                     map (\d -> (d, SyncBlockReq_Throw d (T.pack $ show someException))) [myDepth .. maxDepth]
                  -- We may have pending Result for frames above us,
                  -- and we should throw on them in top to bottom order
                  !maxDepth = case old of
                    [] -> myDepth
                    _ -> maximum (map fst old)
              printSyncBlockReqs "EQ: Left old" old
              printSyncBlockReqs "EQ: Left new" new
              return (new, null old)
            when wasEmpty $ putMVar yieldReadyVar ()
            pure (pred myDepth, newReadyFrames, newRunningThreads)
        -- We're not the top frame, so just store our value so it can be yielded later
        -- In case of exception, kill all running threads and yield the exception
        GT -> case callbackResult of
          Right myRetVal -> do
            putStrLn $ "GT: Right " <> (show myDepth)
            let !newReadyFrames = M.insertWith (error "should be impossible: trying to return from a sync frame that has already returned") myDepth myRetVal oldReadyFrames
                !newRunningThreads = M.delete myDepth oldRunningThreads
            return (oldDepth, newReadyFrames, newRunningThreads)
          Left someException -> do
            putStrLn $ "GT: Left " <> (show myDepth)
            let (!newRunningThreads, toKill) = M.split myDepth oldRunningThreads
            let (!newReadyFrames, _) = M.split myDepth oldReadyFrames
            wasEmpty <- modifyMVar yieldAccumVar $ \old -> do
              -- Cleanup the current and frames above ours
              -- doing this after taking lock over yieldAccumVar
              -- For Haskell, kill all threads
              mapM_ killThread (M.elems toKill)
              -- Drop all pending requests of current and frames above ours
              -- And throw on all frames starting from top
              let !new = (filter ((< pred myDepth) . fst) old) ++
                     map (\d -> (d, SyncBlockReq_Throw d (T.pack $ show someException))) [myDepth .. maxDepth]
                  !maxDepth = case old of
                    [] -> myDepth
                    _ -> maximum (map fst old)
              printSyncBlockReqs "GT: Left old" old
              printSyncBlockReqs "GT: Left new" new
              return (new, null old)
            when wasEmpty $ putMVar yieldReadyVar ()
            pure (pred myDepth, newReadyFrames, newRunningThreads)

      yield :: IO [SyncBlockReq]
      yield = do
        takeMVar yieldReadyVar
        res <- (reverse . (map snd) <$> swapMVar yieldAccumVar [])
        case res of
          [] -> yield
          _ -> pure res
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
      sendReqAsync req = do
        count <- atomically $ do
          modifyTVar' pendingReqs ((:) req)
          c <- readTVar pendingReqsCount
          writeTVar pendingReqsCount (succ c)
          pure (succ c)
        when (count > pendingReqsLimit) $ void $ tryPutMVar sendReqsBatchVar ()
      doSendReqs = forever $ do
        race_ (threadDelay sendReqsTimeout) (takeMVar sendReqsBatchVar)
        reqs <- atomically $ do
          writeTVar pendingReqsCount 0
          reqs <- readTVar pendingReqs
          writeTVar pendingReqs []
          pure $ reverse reqs
        unless (null reqs) $ sendReqsBatch reqs
      env = JSContextRef
        { _jsContextRef_sendReq = sendReqAsync
        , _jsContextRef_sendReqAsync = sendReqAsync
        , _jsContextRef_sendReqsBatchVar = sendReqsBatchVar
        , _jsContextRef_syncThreadId = Nothing
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
        }
      processSyncCommand = \case
        SyncCommand_StartCallback callbackId this args -> do
          mCallback <- fmap (M.lookup callbackId) $ atomically $ readTVar callbacks
          case mCallback of
            Just (callback :: JSVal -> [JSVal] -> JSM JSVal) -> do
              inSyncFrame $ \myDepth threadId -> do
                syncStateLocal <- newMVar SyncState_InSync
                let syncEnv = env { _jsContextRef_sendReq = \req -> enqueueYieldVal (myDepth, SyncBlockReq_Req req)
                                  , _jsContextRef_syncThreadId = Just threadId
                                  , _jsContextRef_syncState = syncStateLocal }
                res <- try $ flip runReaderT syncEnv $ unJSM $
                  (join $ callback <$> wrapJSVal this <*> traverse wrapJSVal args)
                    `catchError` (\v -> unsafePerformIO $
                                   putStrLn "JavaScriptException happened in sync callback" >> throwIO (JavaScriptException v))
                putStrLn $ "end of inSyncFrame: " <> (show myDepth) <> (either show (const "") res)
                pure res
              yield
            Nothing -> error $ "sync callback " <> show callbackId <> " called, but does not exist"
        SyncCommand_Continue -> yield
  void $ forkIO doSendReqs
  return (processRsp, processSyncCommand, env)

#endif
