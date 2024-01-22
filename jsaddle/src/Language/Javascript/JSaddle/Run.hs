{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Exception (try, SomeException(..), throwIO, evaluate)
import Control.Monad (when, join, void, unless, forever)
import Control.Monad.Except (catchError)
import Control.Monad.Trans.Reader (runReaderT, asks)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (atomically, retry)
import Control.Concurrent (myThreadId, forkIO, threadDelay)
import Control.Concurrent.Async (race_, race)
import Control.Concurrent.STM.TVar (writeTVar, readTVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Concurrent.MVar
       (putMVar, takeMVar, newMVar, newEmptyMVar, modifyMVar, modifyMVar_, swapMVar, tryPutMVar, MVar)

import Control.DeepSeq

import Data.Monoid ((<>))
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import GHCJS.Prim.Internal (primToJSVal)

import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Value (valToText)
--TODO: Handle JS exceptions
import Data.Foldable (forM_, traverse_, foldl')
import Language.Javascript.JSaddle.Monad (syncPoint)

import GHC.Stack

-- | The first dynamically-allocated RefId
initialRefId :: RefId
initialRefId = RefId 2

type CallbackResult = Either SomeException (Either JavaScriptException JSVal)

runJavaScript
  :: ([TryReq] -> IO ()) -- ^ Send a batch of requests to the JS engine; we assume that requests are performed in the order they are sent; requests received while in a synchronous block must not be processed until the synchronous block ends (i.e. until the JS side receives the final value yielded back from the synchronous block)
  -> IO ( [Rsp] -> IO () -- Responses must be able to continue coming in as a sync block runs, or else the caller must be careful to ensure that sync blocks are only run after all outstanding responses have been processed
        , SyncCommand -> IO [(SyncCallbackLvl, [SyncBlockReq])]
        , JSContextRef
        )
  -- These default have been determined to give good results on jsaddle-warp
  -- Tested with jsaddle-benchmark (https://github.com/obsidiansystems/jsaddle-benchmark)
runJavaScript = runJavaScriptInt (500 {- 0.5 ms -}) 100

runJavaScriptInt
  :: Int
  -- ^ Timeout for sending async requests in microseconds
  -> Int
  -- ^ Max size of async requests batch size
  -> ([TryReq] -> IO ())
  -- ^ See comments for runJavaScript
  -> IO ( [Rsp] -> IO ()
        , SyncCommand -> IO [(SyncCallbackLvl, [SyncBlockReq])]
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
  pendingSyncReqs <- newTVarIO (mempty :: Map SyncCallbackLvl [req])
  syncState <- newMVar SyncState_InSync
  nextSyncReqId <- newTVarIO $ SyncReqId 1
  syncReqs <- newTVarIO mempty
  sendReqsBatchVar <- newMVar ()
  pendingReqs <- newTVarIO []
  pendingReqsCount <- newTVarIO (0 :: Int)
  threadId <- myThreadId
  let processRsp = traverse_ $ \case
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
        Rsp_CallAsync callbackId fObj this args -> do
          mCallback <- fmap (M.lookup callbackId) $ atomically $ readTVar callbacks
          case mCallback of
            Just callback -> do
              _ <- forkIO $ void $ flip runJSM env $ do
                tid <- liftIO myThreadId
                stackInfo <- liftIO $ renderStack <$> ccsToStrings (_callback_createdAt callback)
                liftIO $ putStrLn $ "Starting callback async on thread " <> show tid <> ":\n" <> stackInfo
                _ <- join $ _callback_value callback <$> wrapJSVal fObj <*> wrapJSVal this <*> traverse wrapJSVal args
                liftIO $ putStrLn $ "Finished callback async on thread " <> show tid <> ":\n" <> stackInfo
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
      enqueueSyncBlockRequest callbackLvl req = atomically $ do
        let addReq = Just . maybe [req] (req :)
        modifyTVar' pendingSyncReqs (M.alter addReq callbackLvl)
      dequeueAllPendingReqs = do
        reqs <- readTVar pendingSyncReqs
        writeTVar pendingSyncReqs mempty
        pure $ map (\(k, v) -> (k, reverse v)) $ filter (\(k, v) -> not $ null v) $ M.assocs $ reqs
      processSyncCommand = \case
        SyncCommand_StartCallback callbackLvl callbackId fObj this args -> do
          mCallback <- fmap (M.lookup callbackId) $ atomically $ readTVar callbacks
          case mCallback of
            Just callback -> do
              threadId <- myThreadId
              syncStateLocal <- newMVar SyncState_InSync
              let syncEnv = env { _jsContextRef_sendReq = \req -> do
                                    -- We MUST fully evaluate our req here, because if we enqueue it while it is not fully evaluated, it could have thunks inside that block on lazy JSVals.  Since we batch requests, the JSVals it's blocked on might be part of the same batch.  This will result in a lockup, since we won't be able to send the batch until we receive responses which can't be sent until after the batch has been sent.
                                    evaluate $ rnf req
                                    enqueueSyncBlockRequest callbackLvl (SyncBlockReq_Req req)
                                , _jsContextRef_syncThreadId = Just threadId
                                , _jsContextRef_myThreadId = threadId
                                , _jsContextRef_syncState = syncStateLocal }
                  run = do
                    tid <- liftIO myThreadId
                    stackInfo <- liftIO $ renderStack <$> ccsToStrings (_callback_createdAt callback)
                    liftIO $ putStrLn $ "Starting callback sync on thread " <> show tid <> ":\n" <> stackInfo
                    result <- ((Right <$>) $ join $ _callback_value callback <$> wrapJSVal fObj <*> wrapJSVal this <*> traverse wrapJSVal args)
                      `catchError` (return . Left)
                    liftIO $ putStrLn $ "Finished callback sync on thread " <> show tid <> ":\n" <> stackInfo
                    pure result
              forkIO $ do
                cbResult :: CallbackResult <- try $ flip runReaderT syncEnv $ unJSM $ run
                enqueueSyncBlockRequest callbackLvl =<< case cbResult of
                  Left e -> pure $ SyncBlockReq_Throw (Left $ T.pack $ show e)
                  -- Even though the valId is escaping, this is safe because we know that our yielded value will
                  -- go out before any potential FreeVal request could go out
                  -- The FreeVal request using this 'env' will be done async after all sync frames.
                  Right v -> flip runReaderT env $ unJSM $ withJSValId (either unJavaScriptException id v) $ \retValId -> do
                    pure $ case v of
                      Left _ -> SyncBlockReq_Throw (Right retValId)
                      Right _ -> SyncBlockReq_Result retValId


              atomically $ do
                allReqs <- readTVar pendingSyncReqs
                case M.lookup callbackLvl allReqs of
                  Nothing -> retry
                  Just _ -> dequeueAllPendingReqs
            Nothing -> error $ "sync callback " <> show callbackId <> " called, but does not exist"
        SyncCommand_Continue -> atomically $ do
          reqs <- dequeueAllPendingReqs
          if null reqs then retry else pure reqs
  void $ forkIO doSendReqs
  return (processRsp, processSyncCommand, env)

#endif
