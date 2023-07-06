{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.WebSockets (
  -- * Running JSM over WebSockets
    jsaddleOr
  , jsaddleApp
  , jsaddleWithAppOr
  , jsaddleAppWithJs
  , jsaddleAppWithJsOr
  , jsaddleAppPartial
  , jsaddleJs
  , jsaddleJs'
) where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, modifyTVar', newTVarIO)
import Control.Exception (handle, AsyncException, throwIO, fromException)

import Data.Monoid ((<>))
import Data.Aeson (encode, decode)

import Network.Wai
       (lazyRequestBody, Application, Request, Response,
        ResponseReceived)
import Network.WebSockets
       (ConnectionOptions(..), Connection, sendTextData,
        receiveDataMessage, acceptRequest, ServerApp, sendPing,
        requestPath, pendingRequest)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.HTTP.Types (Status(..))

import Language.Javascript.JSaddle.Types (JSM(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo, modifyResponse, responseStatus)
import qualified Data.ByteString.Base64.URL as Base64URL
import Data.Text (Text)
import qualified Data.Text as T (pack, drop)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Types as H
       (status403, status200)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (indexHtml, jsaddleCoreJs, ghcjsHelpers)
import Data.Maybe (fromMaybe)
import Data.IORef
       (readIORef, newIORef, atomicModifyIORef')
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (stripPrefix)
import Language.Javascript.JSaddle
       (runJSM, JSVal, TryReq, JSContextRef, SyncBlockReq, SyncCommand, Rsp)
import qualified Data.Map as Map
import System.Entropy (getEntropy)
import Control.Exception (try, SomeException (..))

--TODO: stylish-haskell

import Language.Javascript.JSaddle.WebSockets.Compat (getTextMessageByteString)

type JavascriptSession =
  ( [Rsp] -> IO ()
  , SyncCommand -> IO [(Int, SyncBlockReq)]
  , JSContextRef
  , JSVal
  )

type ConnId = Text

-- | Main Implementation that runs the given JSM Code as a Wai-application.
jsaddleOr :: ConnectionOptions
          -> (JSVal -> JSM ())
          -> Application -- ^ Application that specifies how to handle the routes.
          -> IO Application
jsaddleOr opts entryPoint otherApp = do
    syncFuncs <- newIORef Map.empty
    activeSessionsViaXHR <- newIORef (Map.empty :: Map.Map ConnId (JavascriptSession, MVar ([TryReq] -> IO ()), (TVar [TryReq])))
    let wsApp :: ServerApp
        wsApp pending_conn = do
            let path = decodeUtf8 $ requestPath $ pendingRequest pending_conn
            conn <- acceptRequest pending_conn
            let sendTryReqs = sendTextData conn . encode
            wsAppWithSession conn =<< if path == "/"
              then do
                -- Start a new session
                sendTryReqsFMVar <- newMVar sendTryReqs
                (connId, session) <- startNewSession (sendTextData conn) sendTryReqsFMVar
                pure (connId, session)
              else do
                -- Connect to an existing session started via begin-session XHR
                let connId = T.drop 1 path -- should be "/" followed by connId
                m <- readIORef activeSessionsViaXHR
                case Map.lookup connId m of
                  Nothing -> error "connId not found"
                  Just (session, sendTryReqsFMVar, tryReqsToBeDone) -> do
                    -- switch from XHR to websocket for sending TryReqs
                    modifyMVar_ sendTryReqsFMVar $ \_ -> do
                      rs <- atomically $ do
                        rs <- readTVar tryReqsToBeDone
                        writeTVar tryReqsToBeDone []
                        pure rs
                      sendTryReqs rs
                      pure sendTryReqs
                    pure (connId, session)

        wsAppWithSession conn (connId, (processResult, _, _, _))= do
            _ <- forkIO . forever $
                receiveDataMessage conn >>= \msg -> case getTextMessageByteString msg of
                    Just t -> case decode t of
                        Nothing -> putStrLn $ "jsaddle response decode failed: " <> show t
                        Just r  -> do
                          result <- try $ processResult r
                          case result of
                            Left e@(SomeException _) -> putStrLn $ "jsaddle processResult failed: " <> show e
                            Right _ -> return ()
                    _ -> error "jsaddle WebSocket unexpected binary data"
            waitTillClosed conn
            atomicModifyIORef' syncFuncs $ \fs -> (Map.delete connId fs, ())
            -- TODO: cleanup session in case of xhr only session
            atomicModifyIORef' activeSessionsViaXHR $ \fs -> (Map.delete connId fs, ())

        startNewSession :: (ConnId -> IO ()) -> MVar ([TryReq] -> IO ()) -> IO (ConnId, JavascriptSession)
        startNewSession sendConnId sendTryReqsFMVar = do
            connId <- decodeUtf8 . Base64URL.encode <$> getEntropy 24
            sendConnId connId
            session@(processResult, processSyncCommand, env, arg) <- runJavaScript $ \req -> do
              sendTryReqsF <- readMVar sendTryReqsFMVar
              sendTryReqsF req
            atomicModifyIORef' syncFuncs $ \fs ->
              ( Map.insertWith (error $ "duplicate connection ID" <> show connId) connId processSyncCommand fs
              , ()
              )
            forkIO $ try (runJSM (entryPoint arg) env) >>= \case
              Left e@(SomeException _) -> putStrLn $ "done: left: " <> show e
              Right _ -> putStrLn $ "done: right"
            pure (connId, session)

        -- Based on Network.WebSocket.forkPingThread
        waitTillClosed conn = ignore `handle` go 1
          where
            go :: Int -> IO ()
            go i = do
                threadDelay (1 * 1000 * 1000)
                sendPing conn (T.pack $ show i)
                go (i + 1)

        ignore e = case fromException e of
            Just async -> throwIO (async :: AsyncException)
            Nothing    -> return ()

        syncHandler :: Application
        syncHandler req sendResponse = case (W.requestMethod req, W.pathInfo req) of
            ("OPTIONS", _) -> do
              sendResponse $ W.responseLBS
                H.status200
                [ ("Allow", "OPTIONS, POST")
                , ("Access-Control-Allow-Origin", "*")
                , ("Access-Control-Allow-Methods", "OPTIONS, POST")
                , ("Access-Control-Allow-Headers", "content-type")
                ]
                ""
            ("POST", ["begin-session"]) -> do
              tryReqsToBeDone <- newTVarIO mempty
              sendTryReqsFMVar <- newMVar $ \reqs ->
                atomically $ modifyTVar' tryReqsToBeDone (<> reqs)
              (connId, session@(processResult, processSyncCommand, env, arg)) <- startNewSession (const $ pure ()) sendTryReqsFMVar
              atomicModifyIORef' activeSessionsViaXHR $ \fs ->
                ( Map.insertWith (error $ "duplicate connection ID" <> show connId) connId (session, sendTryReqsFMVar, tryReqsToBeDone) fs
                , ()
                )
              sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] $ encode connId

            ("POST", ["send-rsp", connId]) -> do
              body <- lazyRequestBody req
              Just ((processResult, _, _, _), _, _) <- Map.lookup connId <$> readIORef activeSessionsViaXHR
              _ <- forkIO $ case decode body of
                Nothing -> putStrLn $ "jsaddle response decode failed: " <> show body
                Just r  -> do
                  result <- try $ processResult r
                  case result of
                    Left e@(SomeException _) -> putStrLn $ "jsaddle processResult failed: " <> show e
                    Right _ -> return ()
              sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] $ encode connId

            ("POST", ["get-try-reqs", connId]) -> do
              Just (_, _, tryReqsToBeDone) <- Map.lookup connId <$> readIORef activeSessionsViaXHR
              rs <- atomically $ do
                rs <- readTVar tryReqsToBeDone
                writeTVar tryReqsToBeDone []
                pure rs
              sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] $ encode rs

            ("POST", ["sync", connId]) -> do
                Just syncFunc <- Map.lookup connId <$> readIORef syncFuncs
                body <- lazyRequestBody req
                case decode body of
                    Nothing -> error $ "jsaddle sync message decode failed: " <> show body
                    Just parsed -> do
                      result <- syncFunc parsed
                      sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json"), ("Access-Control-Allow-Origin", "*")] $ encode result
            (method, _) -> (catch404 otherApp) req sendResponse
              where catch404 = W.modifyResponse $ \resp ->
                      case (method, W.responseStatus resp) of
                        ("GET", Status 404 _) -> indexResponse
                        _ -> resp
    return $ websocketsOr opts wsApp syncHandler

--------------------------------------------------------------------------------
-- * Applications

-- | The default jsaddle application, i.e. with the default JSAddle
-- initialization javascript.
jsaddleApp :: Application
jsaddleApp = jsaddleAppWithJs $ jsaddleJs False

-- | Creates a JSAddle application that accepts only the index, and
-- replies forbidden to all other requests.
jsaddleAppWithJs :: ByteString -- ^ the javascript to initialize JSAddle
                 -> Application
jsaddleAppWithJs js req sendResponse =
  jsaddleAppWithJsOr js
    (\_ _ -> sendResponse $ W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden")
    req sendResponse

-- | Serves JSAddle, any other requests are handled by the given other
-- application.
jsaddleAppWithJsOr :: ByteString -> Application -> Application
jsaddleAppWithJsOr js otherApp req sendResponse =
  fromMaybe (otherApp req sendResponse)
    (jsaddleAppPartialWithJs js req sendResponse)

-- | Specify the JSM code we want to run when starting the JSAddle application.
jsaddleWithAppOr :: ConnectionOptions -> (JSVal -> JSM ()) -> Application -> IO Application
jsaddleWithAppOr opts entryPoint otherApp = jsaddleOr opts entryPoint $ \req sendResponse ->
  (fromMaybe (otherApp req sendResponse)
     (jsaddleAppPartial req sendResponse))

-- | JSAddle application that accepts only GET requests on the following paths
-- - /
-- - /jsaddle.js
--
-- if the response matches either of those, we serve them.
jsaddleAppPartial :: Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartial = jsaddleAppPartialWithJs $ jsaddleJs False


-- | Implementation of jsaddleAppPartial that also takes the bytestring representing the
-- jsaddle initialization we should return.
jsaddleAppPartialWithJs :: ByteString -> Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartialWithJs js req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse indexResponse
    ("OPTIONS", ["jsaddle.js"]) -> Just $ do
      sendResponse $ W.responseLBS
        H.status200
        [ ("Allow", "OPTIONS, GET")
        , ("Access-Control-Allow-Origin", "*")
        , ("Access-Control-Allow-Methods", "OPTIONS, POST")
        , ("Access-Control-Allow-Headers", "content-type")
        ]
        ""
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript"), ("Access-Control-Allow-Origin", "*")] js
    _ -> Nothing

-- | Respond with the index html page
indexResponse :: Response
indexResponse = W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml

--------------------------------------------------------------------------------
-- * The piece of javascript to to initialize JSAddle

-- | The javascript, the boolean indicates whether we shoulld refresh on reload.
jsaddleJs :: Bool -> ByteString
jsaddleJs = jsaddleJs' Nothing

--TODO: Make refreshOnLoad work
-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
--
-- |  The javascript file that sets up the connection to the JSAddle Application.
jsaddleJs' :: Maybe ByteString -- ^ URI corresponding to JSAddle
           -> Bool -- ^ should we refresh on reload
           -> ByteString
jsaddleJs' jsaddleUri refreshOnLoad = jsaddleCoreJs <> "\
    \if(typeof global !== \"undefined\" && typeof require === \"function\") {\n\
    \    global.window = global;\n\
    \    global.WebSocket = require('ws');\n\
    \}\n\
    \\n\
    \var connectWebsocket = function(o) {\n\
    \    var wsaddress = (typeof(JSADDLE_ROOT) !== 'undefined') ? JSADDLE_ROOT.replace('http', 'ws') : "
      <> maybe "window.location.protocol.replace('http', 'ws')+\"//\"+window.location.hostname+(window.location.port?(\":\"+window.location.port):\"\")"
            (\ s -> "\"ws" <> s <> "\"")
            (jsaddleUri >>= LBS.stripPrefix "http")
      <> ";\n\
    \\n\
    \    var ws = new WebSocket(o.connId ? wsaddress + '/' + o.connId: wsaddress);\n\
    \    var connId = o.connId ? o.connId : undefined;\n\
    \    var sync = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', ((typeof(JSADDLE_ROOT) !== 'undefined') ? JSADDLE_ROOT : '" <> fromMaybe "" jsaddleUri <> "') + '/sync/' + connId, false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify(v));\n\
    \      return JSON.parse(xhr.responseText);\n\
    \    };\n\
    \\n\
    \    ws.onopen = function(e) {\n\
    \        var core;\n\
    \        if (o.core) {\n\
    \            core = o.core;\n\
    \            core.internals.sendRsp = function(a) {\n\
    \              ws.send(JSON.stringify(a));\n\
    \            };\n\
    \            ws.onmessage = function(e) {\n\
    \                core.processReqs(JSON.parse(e.data));\n\
    \            };\n\
    \        } else {\n\
    \            core = jsaddleCoreJs(window, function(a) {\n\
    \              ws.send(JSON.stringify(a));\n\
    \            }, sync, 10 /* RESPONSE_BUFFER_MAX_SIZE (0 to disable) */, (typeof(arg) !== 'undefined') ? arg : undefined);\n\
    \            ws.onmessage = function(c) {\n\
    \                connId = c.data;\n\
    \                ws.onmessage = function(e) {\n\
    \                    core.processReqs(JSON.parse(e.data));\n\
    \                };\n\
    \            }\n\
    \        }\n\
    \    };\n\
    \\n\
    \    ws.onerror = function() {\n\
    \        setTimeout(connect, 1000);\n\
    \    };\n\
    \}\n\
    \\n\
    \var connectXHR = function() {\n\
    \    var beginSession = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', ((typeof(JSADDLE_ROOT) !== 'undefined') ? JSADDLE_ROOT : '" <> fromMaybe "" jsaddleUri <> "') + '/begin-session', false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify());\n\
    \      return JSON.parse(xhr.responseText);\n\
    \    };\n\
    \    var connId = beginSession();\n\
    \    var sync = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', ((typeof(JSADDLE_ROOT) !== 'undefined') ? JSADDLE_ROOT : '" <> fromMaybe "" jsaddleUri <> "') + '/sync/' + connId, false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify(v));\n\
    \      return JSON.parse(xhr.responseText);\n\
    \    };\n\
    \    var sendRsp = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', ((typeof(JSADDLE_ROOT) !== 'undefined') ? JSADDLE_ROOT : '" <> fromMaybe "" jsaddleUri <> "') + '/send-rsp/' + connId,false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify(v));\n\
    \      return;\n\
    \    };\n\
    \\n\
    \    var core = jsaddleCoreJs(window, sendRsp, sync, 10 /* RESPONSE_BUFFER_MAX_SIZE (0 to disable) */, (typeof(arg) !== 'undefined') ? arg : undefined);\n\
    \\n\
    \    var processReqsViaXHR = function() {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', ((typeof(JSADDLE_ROOT) !== 'undefined') ? JSADDLE_ROOT : '" <> fromMaybe "" jsaddleUri <> "') + '/get-try-reqs/' + connId,false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send();\n\
    \      core.processReqs(JSON.parse(xhr.responseText));\n\
    \      return;\n\
    \    };\n\
    \\n\
    \    return { connId, core, processReqsViaXHR };\n\
    \}\n\
    \\n\
    \ " <> ghcjsHelpers <> "\
    \if (typeof dontAutoConnectWebsocket === 'boolean' ? !dontAutoConnectWebsocket: true) { connectWebsocket({}) };\n\
    \"
