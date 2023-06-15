{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (void, forever)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets (jsaddleJs', jsaddleAppWithJs, jsaddleOr)

import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (readProcess, system)

import Test.Hspec

main :: IO ()
main = do
  putStrLn "Running jsaddle-warp spec"
  system "node --version" >>= \case
    ExitSuccess -> return ()
    e           -> do
      putStrLn "node not found"
      exitWith e
  context <- newEmptyMVar
  let f = do
            liftIO $ tryTakeMVar context
            liftIO . putMVar context =<< askJSM
            liftIO . forever $ threadDelay 1000000
  forkIO $ runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions f jsaddleApp

  forkIO $ void $ readProcess "node" ["jsaddle-warp/node-client/index.js"] "" >>= putStr
  hspec $ aroundAll (bracket (takeMVar context) (putMVar context)) spec

  where
    f1 = do
        v <- eval ("'Hello World'.length")
        valToText v >>= liftIO . putStrLn . T.unpack
        liftIO $ threadDelay $ 2*1000*1000
        eval ("process.exit()")
        pure ()
        -- hspec spec
    port = 3709
    uri = BS.pack $ "http://0.0.0.0:" <> show port
    jsaddleApp = jsaddleAppWithJs (jsaddleJs' (Just uri) False)

spec :: SpecWith JSContextRef
spec = do
  describe "Object Spec" $ do
    it "Lookup a property based on its name." $ \ctx -> do
      result <- flip runJSM ctx $ do
        valToText =<< val "Hello World" ! "length"
      result `shouldBe` (T.pack "11")

  describe "Bugs" $ do
    it "does not get deadlocked when making use of JSVal just created" $ \ctx -> do
      result <- flip runJSM ctx $ do
        (callbackId, jsVal) <- newSyncCallback'' $ \_ _ [arg] -> do
          _ <- (global ! "console") # "log" $ ["Starting Test"]
          myPropsJson <- valToJSON arg
          (global ! "console") # "log" $ [toJSVal myPropsJson]
        o <- obj
        (o <# "x") "Hello";
        call (Object jsVal) o [o] `catchError`
          \(JavaScriptException e) -> do
              msg <- valToText e
              liftIO $ putStrLn $ "Error: " <> T.unpack msg
              pure e
        valToText =<< val "Hello World" ! "length"
      result `shouldBe` (T.pack "11")

