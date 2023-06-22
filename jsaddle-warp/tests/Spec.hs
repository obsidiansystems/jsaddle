{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as BS

import Language.Javascript.JSaddle (askJSM)
import Language.Javascript.JSaddleSpec (spec)
import Language.Javascript.JSaddle.WebSockets (jsaddleJs', jsaddleAppWithJs, jsaddleOr)

import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import System.Directory (doesDirectoryExist)
import System.Exit (exitWith, ExitCode(..))
import System.Process (system, spawnProcess, terminateProcess)

import Test.Hspec (hspec, around, mapSubject)

main :: IO ()
main = do
  nodeClientPath <- setupNodeClient
  context <- newEmptyMVar
  let
    f _ = do
      _ <- liftIO $ tryTakeMVar context
      liftIO . putMVar context =<< askJSM
      liftIO . forever $ threadDelay maxBound

    port = 13709
    uri = BS.pack $ "http://0.0.0.0:" <> show port
    jsaddleApp = jsaddleAppWithJs (jsaddleJs' (Just uri) False)

  void $ forkIO $ runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions f jsaddleApp


  let
    -- Runs a new node client for each test
    getContext = do
      ph <- spawnProcess "node" [nodeClientPath, show port]
      ctx <- takeMVar context
      return (ph, ctx)
    releaseContext (ph, _) = do
      terminateProcess ph
  hspec $ around (bracket getContext releaseContext) (mapSubject snd spec)


setupNodeClient :: IO (FilePath)
setupNodeClient = do
  system "node --version" >>= \case
    ExitSuccess -> return ()
    e           -> do
      putStrLn "'node' not found"
      exitWith e

  -- The 'cabal test' could be running from root of jsaddle repo, so adjust the path
  ncExist <- doesDirectoryExist "node-client"
  jwExist <- doesDirectoryExist "jsaddle-warp/node-client"
  unless (ncExist || jwExist) $ do
    putStrLn "node-client directory not found"
    exitWith (ExitFailure 1)

  let nodeClientDir = if ncExist then "node-client" else "jsaddle-warp/node-client"

  nmExist <- doesDirectoryExist (nodeClientDir <> "/node_modules")
  unless nmExist $ do
    system "npm --version" >>= \case
      ExitSuccess -> return ()
      e           -> do
        putStrLn "'npm' not found"
        exitWith e

    system ("npm install --prefix " <> nodeClientDir) >>= \case
      ExitSuccess -> return ()
      e           -> do
        putStrLn "'npm install' did not succeed"
        exitWith e
  return nodeClientDir
