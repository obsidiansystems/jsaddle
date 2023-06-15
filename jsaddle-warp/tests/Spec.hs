{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T

import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets (jsaddleJs', jsaddleAppWithJs, jsaddleOr)

import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)
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
  forkIO $ runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions f jsaddleApp

  forkIO $ void $ readProcess "node" ["jsaddle-warp/node-client/index.js"] "" >>= putStr
  liftIO $ threadDelay $ 4*1000*1000
  putStrLn "Done jsaddle-warp spec"

  where f = do
            v <- eval ("'Hello World'.length" :: T.Text)
            valToText v >>= liftIO . putStrLn . T.unpack
            liftIO $ threadDelay $ 2*1000*1000
            eval ("process.exit()" :: T.Text)
            pure ()
            -- hspec spec
        port = 3709
        jsaddleApp = jsaddleAppWithJs (jsaddleJs' (Just "http://0.0.0.0:3709") False)

spec = do
  describe "Prelude.read" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)
