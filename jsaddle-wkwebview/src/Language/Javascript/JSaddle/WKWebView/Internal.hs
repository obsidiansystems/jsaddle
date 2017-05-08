{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Language.Javascript.JSaddle.WKWebView.Internal
    ( jsaddleMain
    , jsaddleMainFile
    , WKWebView(..)
    , mainBundleResourcePath
    , boolToCChar
    , ccharToBool
    , AppDelegateConfig(..)
    , AppDelegateNotificationConfig(..)
    , AuthorizationOption (..)
    -- , authorizationOptionEnum
    ) where

import Control.Monad (void, join)
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import Data.Default
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (useAsCString, packCString)
import qualified Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Aeson (encode, decode)

import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr)

import Language.Javascript.JSaddle (Results, JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

newtype WKWebView = WKWebView (Ptr WKWebView)

foreign export ccall jsaddleStart :: StablePtr (IO ()) -> IO ()
foreign export ccall jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
foreign export ccall withWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
foreign export ccall handler :: StablePtr (IO ()) -> IO ()
foreign export ccall handlerCString :: CString -> StablePtr (CString -> IO ()) -> IO ()
foreign export ccall handlerCChar :: CChar -> StablePtr (CChar -> IO ()) -> IO ()
foreign import ccall addJSaddleHandler :: WKWebView -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> IO ()
foreign import ccall loadHTMLString :: WKWebView -> CString -> IO ()
foreign import ccall loadBundleFile :: WKWebView -> CString -> CString -> IO ()
foreign import ccall evaluateJavaScript :: WKWebView -> CString -> IO ()
foreign import ccall mainBundleResourcePathC :: IO CString
-- foreign import ccall "authorizationOptionBadge" c_UNAuthorizationOptionBadge :: CInt
-- foreign import ccall "authorizationOptionSound" c_UNAuthorizationOptionSound :: CInt
-- foreign import ccall "authorizationOptionAlert" c_UNAuthorizationOptionAlert :: CInt
-- foreign import ccall "authorizationOptionCarPlay" c_UNAuthorizationOptionCarPlay :: CInt

-- | Run JSaddle in WKWebView
jsaddleMain :: JSM () -> WKWebView -> IO ()
jsaddleMain f webView = do
    jsaddleMain' f webView $
        useAsCString (toStrict indexHtml) $ loadHTMLString webView

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
jsaddleMainFile :: BS.ByteString -- ^ The file to navigate to.
                -> BS.ByteString -- ^ The path to allow read access to.
                -> JSM () -> WKWebView -> IO ()
jsaddleMainFile url allowing f webView = do
    jsaddleMain' f webView $
        useAsCString url $ \u ->
            useAsCString allowing $ \a ->
                loadBundleFile webView u a

jsaddleMain' :: JSM () -> WKWebView -> IO () -> IO ()
jsaddleMain' f webView loadHtml = do
    ready <- newEmptyMVar

    (processResult, start) <- runJavaScript (\batch ->
        useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $
            evaluateJavaScript webView)
        f

    startHandler <- newStablePtr (putMVar ready ())
    resultHandler <- newStablePtr processResult
    addJSaddleHandler webView startHandler resultHandler
    loadHtml
    void . forkOS $ do
        takeMVar ready
        useAsCString (toStrict jsaddleJs) (evaluateJavaScript webView) >> void (forkIO start)

jsaddleStart :: StablePtr (IO ()) -> IO ()
jsaddleStart ptr = join (deRefStablePtr ptr)

jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
jsaddleResult ptrHandler s = do
    processResult <- deRefStablePtr ptrHandler
    result <- packCString s
    case decode (fromStrict result) of
        Nothing -> error $ "jsaddle WebSocket decode failed : " <> show result
        Just r  -> processResult r

withWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
withWebView w ptr = do
    f <- deRefStablePtr ptr
    f w

handler :: StablePtr (IO ()) -> IO ()
handler ptr = join (deRefStablePtr ptr)

handlerCString :: CString -> StablePtr (CString -> IO ()) -> IO ()
handlerCString c ptr = do
    f <- deRefStablePtr ptr
    f c

handlerCChar :: CChar -> StablePtr (CChar -> IO ()) -> IO ()
handlerCChar c ptr = do
    f <- deRefStablePtr ptr
    f c

-- | AppDelegate configuration: Toggles UIApplicationDelegate features on and
--   off and passes handler actions to the appropriate callbacks in the foreign
--   AppDelegate code
-- https://developer.apple.com/reference/uikit/uiapplicationdelegate
data AppDelegateConfig = AppDelegateConfig
    -- { _appDelegateConfig_willFinishLaunchingWithOptions :: IO ()
    -- , _appDelegateConfig_didFinishLaunchingWithOptions :: IO ()
    -- , _appDelegateConfig_applicationDidBecomeActive :: IO ()
    -- , _appDelegateConfig_applicationDidEnterBackground :: IO ()
    -- , _appDelegateConfig_applicationWillResignActive :: IO ()
    -- , _appDelegateConfig_applicationWillEnterForeground :: IO ()
    -- , _appDelegateConfig_applicationWillTerminate :: IO ()
    -- , _appDelegateConfig_applicationProtectedDataWillBecomeUnavailable :: IO ()
    -- , _appDelegateConfig_applicationProtectedDataDidBecomeUnavailable :: IO ()
    -- , _appDelegateConfig_applicationDidReceiveMemoryWarning :: IO ()
    -- , _appDelegateConfig_applicationSignificantTimeChange :: IO ()
    { _appDelegateConfig_appDelegateNotificationConfig :: AppDelegateNotificationConfig
    }

instance Default AppDelegateConfig where
  def = AppDelegateConfig
    -- { _appDelegateConfig_willFinishLaunchingWithOptions = return ()
    -- , _appDelegateConfig_didFinishLaunchingWithOptions = return ()
    -- , _appDelegateConfig_applicationDidBecomeActive = return ()
    -- , _appDelegateConfig_applicationDidEnterBackground = return ()
    -- , _appDelegateConfig_applicationWillResignActive = return ()
    -- , _appDelegateConfig_applicationWillEnterForeground = return ()
    -- , _appDelegateConfig_applicationWillTerminate = return ()
    -- , _appDelegateConfig_applicationProtectedDataWillBecomeUnavailable = return ()
    -- , _appDelegateConfig_applicationProtectedDataDidBecomeUnavailable = return ()
    -- , _appDelegateConfig_applicationDidReceiveMemoryWarning = return ()
    -- , _appDelegateConfig_applicationSignificantTimeChange = return ()
    { _appDelegateConfig_appDelegateNotificationConfig = def
    }

data AuthorizationOption = AuthorizationOption_Badge
                         | AuthorizationOption_Sound
                         | AuthorizationOption_Alert
                         | AuthorizationOption_CarPlay
  deriving (Eq, Ord)

-- authorizationOptionEnum :: AuthorizationOption -> CInt
-- authorizationOptionEnum o = case o of
--     AuthorizationOption_Badge -> c_UNAuthorizationOptionBadge
--     AuthorizationOption_Sound -> c_UNAuthorizationOptionSound
--     AuthorizationOption_Alert -> c_UNAuthorizationOptionAlert
--     AuthorizationOption_CarPlay -> c_UNAuthorizationOptionCarPlay

-- | User notification configuration: Toggles user notification features and
--   provides a way to perform actions in notification-related callbacks.
-- https://developer.apple.com/reference/usernotifications/unusernotificationcenter
data AppDelegateNotificationConfig = AppDelegateNotificationConfig
    -- { _appDelegateNotificationConfig_requestAuthorizationWithOptions :: Bool
    -- -- , _appDelegateNotificationConfig_requestAuthorizationCompletionHandler :: Bool -> IO ()
    -- , _appDelegateNotificationConfig_registerForRemoteNotifications :: Bool
    { _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken :: CString -> IO ()
    -- , _appDelegateNotificationConfig_didFailToRegisterForRemoteNotifcationsWithError :: CString -> IO ()
    }

instance Default AppDelegateNotificationConfig where
    def = AppDelegateNotificationConfig
      -- { _appDelegateNotificationConfig_requestAuthorizationWithOptions = False
      -- , _appDelegateNotificationConfig_requestAuthorizationCompletionHandler = \_ -> return ()
      -- , _appDelegateNotificationConfig_registerForRemoteNotifications = False
      { _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken = \_ -> return ()
      -- , _appDelegateNotificationConfig_didFailToRegisterForRemoteNotifcationsWithError = \_ -> return ()
      }

ccharToBool :: CChar -> Bool
ccharToBool a = a /= 0

boolToCChar :: Bool -> CChar
boolToCChar a = if a then 1 else 0

jsaddleJs :: ByteString
jsaddleJs = ghcjsHelpers <> "\
    \runJSaddleBatch = (function() {\n\
    \ " <> initState <> "\n\
    \ return function(batch) {\n\
    \ " <> runBatch (\a -> "window.webkit.messageHandlers.jsaddle.postMessage(JSON.stringify(" <> a <> "));") <> "\
    \ };\n\
    \})();\n\
    \"

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \</html>"

mainBundleResourcePath :: IO (Maybe BS.ByteString)
mainBundleResourcePath = do
    bs <- mainBundleResourcePathC
    if bs == nullPtr
        then return Nothing
        else Just <$> packCString bs

