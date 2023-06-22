module Language.Javascript.JSaddleSpec where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T
import Language.Javascript.JSaddle

import qualified Language.Javascript.JSaddle.ObjectSpec as ObjectSpec
import qualified Language.Javascript.JSaddle.ValueSpec as ValueSpec

import Test.Hspec

spec :: SpecWith JSContextRef
spec = do
  describe "Miscellaneous" misc
  describe "ObjectSpec" ObjectSpec.spec
  describe "ValueSpec" ValueSpec.spec

misc :: SpecWith JSContextRef
misc = do
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
