module Language.Javascript.JSaddleSpec where

import Prelude hiding ((!!))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T
import Language.Javascript.JSaddle

import qualified Language.Javascript.JSaddle.ObjectSpec as ObjectSpec
import qualified Language.Javascript.JSaddle.RunSpec as RunSpec
import qualified Language.Javascript.JSaddle.ValueSpec as ValueSpec

import Test.Hspec

spec :: SpecWith JSContextRef
spec = do
  describe "Miscellaneous" misc
  describe "ObjectSpec" ObjectSpec.spec
  describe "ValueSpec" ValueSpec.spec
  describe "RunSpec" RunSpec.spec

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

    it "Creates an array containing a single number" $ \ctx -> do
      -- The Array constructor has a special case for a single number
      -- which is not the behaviour we expect when we create an array with a single number
      -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Array#parameters
      -- Make sure we construct the array containing the specified number
      result <- flip runJSM ctx $ valToText =<< (array [5::Int] !! 0)
      result `shouldBe` (T.pack "5")
