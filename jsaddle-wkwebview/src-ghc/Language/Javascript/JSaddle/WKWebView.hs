module Language.Javascript.JSaddle.WKWebView
    ( jsaddleMain
    , jsaddleMainFile
    , WKWebView(..)
    , run
    , runFile
    , mainBundleResourcePath
    ) where

import Data.Bits ((.|.), zeroBits)
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMain, jsaddleMainFile, WKWebView(..), mainBundleResourcePath, AppDelegateConfig(..), AppDelegateNotificationConfig(..), boolToCChar, ccharToBool) -- , authorizationOptionEnum)
import System.Environment (getProgName)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall safe runInWKWebView :: StablePtr (WKWebView -> IO ())
                                         -> CString
                                         -- -> StablePtr (IO ()) -- willFinishLaunchingWithOptions
                                         -- -> StablePtr (IO ()) -- didFinishLaunchingWithOptions
                                         -- -> StablePtr (IO ()) -- applicationDidBecomeActive
                                         -- -> StablePtr (IO ()) -- applicationDidEnterBackground
                                         -- -> StablePtr (IO ()) -- applicationWillResignActive
                                         -- -> StablePtr (IO ()) -- applicationWillEnterForeground
                                         -- -> StablePtr (IO ()) -- applicationWillTerminate
                                         -- -> StablePtr (IO ()) -- applicationProtectedDataWillBecomeUnavailable
                                         -- -> StablePtr (IO ()) -- applicationProtectedDataDidBecomeUnavailable
                                         -- -> StablePtr (IO ()) -- applicationDidReceiveMemoryWarning
                                         -- -> StablePtr (IO ()) -- applicationSignificantTimeChange
                                         -- -> CChar -- whether to run requestAuthorizationWithOptions
                                         -- -> CInt -- options to pass to requestAuthorizationWithOptions
                                         -- -> StablePtr (CChar -> IO ()) -- requestAuthorizationCompletionHandler
                                         -- -> CChar -- whether to run registerForRemoteNotifications
                                         -> StablePtr (CString -> IO ()) -- didRegisterForRemoteNotificationsWithDeviceTokenCallback
                                         -- -> StablePtr (CString -> IO ()) -- didFailToRegisterForRemoteNotifcationsWithError
                                         -> IO ()

-- | Run JSaddle in a WKWebView
run :: AppDelegateConfig -> JSM () -> IO ()
run cfg f = run' cfg Nothing f

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
runFile :: AppDelegateConfig
        -> ByteString -- ^ The file to navigate to.
        -> ByteString -- ^ The path to allow read access to.
        -> JSM () -> IO ()
runFile cfg url allowing f = run' cfg (Just (url, allowing)) f

run' :: AppDelegateConfig -- ^ Application configuration
     -> Maybe (ByteString, ByteString) -- ^ File to navigate to and path to allow read access to
     -> JSM ()
     -> IO ()
run' cfg mFile f = do
    jsaddleHandler <- newStablePtr $! case mFile of
        Nothing -> jsaddleMain f
        Just (url, allowing) -> jsaddleMainFile url allowing f
    progName <- getProgName

    -- AppDelegate callbacks
    -- willFinishLaunchingWithOptions <- newStablePtr $! _appDelegateConfig_willFinishLaunchingWithOptions cfg
    -- didFinishLaunchingWithOptions <- newStablePtr $! _appDelegateConfig_didFinishLaunchingWithOptions cfg
    -- applicationDidBecomeActive <- newStablePtr $! _appDelegateConfig_applicationDidBecomeActive cfg
    -- applicationDidEnterBackground <- newStablePtr $! _appDelegateConfig_applicationDidEnterBackground cfg
    -- applicationWillResignActive <- newStablePtr $! _appDelegateConfig_applicationWillResignActive cfg
    -- applicationWillEnterForeground <- newStablePtr $! _appDelegateConfig_applicationWillEnterForeground cfg
    -- applicationWillTerminate <- newStablePtr $! _appDelegateConfig_applicationWillTerminate cfg
    -- applicationProtectedDataWillBecomeUnavailable <- newStablePtr $! _appDelegateConfig_applicationProtectedDataWillBecomeUnavailable cfg
    -- applicationProtectedDataDidBecomeUnavailable <- newStablePtr $! _appDelegateConfig_applicationProtectedDataDidBecomeUnavailable cfg
    -- applicationDidReceiveMemoryWarning <- newStablePtr $! _appDelegateConfig_applicationDidReceiveMemoryWarning cfg
    -- applicationSignificantTimeChange <- newStablePtr $! _appDelegateConfig_applicationSignificantTimeChange cfg

    -- Request authorization to show user-facing notifications (e.g., Badges, Alerts)
    let ncfg = _appDelegateConfig_appDelegateNotificationConfig cfg
        -- (requestNotificationAuth, requestedNotificationOptions) = case Set.toList $ _appDelegateNotificationConfig_requestAuthorizationWithOptions ncfg of
        --   [] -> (boolToCChar False, zeroBits)
        --   opts -> (boolToCChar True, foldr (.|.) zeroBits $ authorizationOptionEnum <$> opts)
    -- requestAuthorizationCompletionHandler <- newStablePtr $! _appDelegateNotificationConfig_requestAuthorizationCompletionHandler ncfg . ccharToBool
        -- requestNotificationAuth = boolToCChar $ _appDelegateNotificationConfig_requestAuthorizationWithOptions ncfg
    -- Register for push notifications
    -- let registerForRemoteNotifications = boolToCChar $ _appDelegateNotificationConfig_registerForRemoteNotifications ncfg
    -- didRegisterForRemoteNotificationsWithDeviceTokenCallback <- newStablePtr $! _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken ncfg
    -- didFailToRegisterForRemoteNotifcationsWithError <- newStablePtr $! _appDelegateNotificationConfig_didFailToRegisterForRemoteNotifcationsWithError ncfg

    didRegisterForRemoteNotificationsWithDeviceTokenCallback <- newStablePtr printCString
    withCString progName $ \pn -> runInWKWebView
        jsaddleHandler
        pn
        -- willFinishLaunchingWithOptions
        -- didFinishLaunchingWithOptions
        -- applicationDidBecomeActive
        -- applicationDidEnterBackground
        -- applicationWillResignActive
        -- applicationWillEnterForeground
        -- applicationWillTerminate
        -- applicationProtectedDataWillBecomeUnavailable
        -- applicationProtectedDataDidBecomeUnavailable
        -- applicationDidReceiveMemoryWarning
        -- applicationSignificantTimeChange
        -- requestNotificationAuth
        -- requestedNotificationOptions
        -- requestAuthorizationCompletionHandler
        -- registerForRemoteNotifications
        didRegisterForRemoteNotificationsWithDeviceTokenCallback
        -- didFailToRegisterForRemoteNotifcationsWithError

printCString :: CString -> IO ()
printCString x = putStrLn =<< peekCString x
