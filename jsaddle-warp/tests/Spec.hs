{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T

import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets (jsaddleJs', jsaddleAppWithJs, jsaddleOr)

import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Test.Hspec

main :: IO ()
main =
  runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions f jsaddleApp
  where f = do
            v <- eval ("'Hello World'.length" :: T.Text)
            valToText v >>= liftIO . putStrLn . T.unpack
            -- hspec spec
        port = 3709
        jsaddleApp = jsaddleAppWithJs (jsaddleJs' (Just "http://0.0.0.0:3709") False)

spec = do
  describe "Prelude.read" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)
