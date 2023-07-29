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
  let
    resultShouldBe res m ctx = do
      result <- runJSM (valToText =<< m) ctx
      result `shouldBe` (T.pack res)

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

  describe "Sync callbacks" $ do
    it "should block" $
      resultShouldBe "1" $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f1 <- function $ \_ _ _ -> do
          (o <# k) (1 :: Int)
          pure ()
        call f1 f1 ()
        o ! k

    it "should block when nested" $
      resultShouldBe "2" $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f1 <- function $ \_ _ _ -> do
          (o <# k) (1 :: Int)
          pure ()
        Function _ f2 <- function $ \_ _ _ -> do
          call f1 f1 ()
          (o <# k) (2 :: Int)
          pure ()
        call f2 f2 ()
        o ! k

    it "should block when nested 2" $
      resultShouldBe "1" $ do
        o <- create
        let k = "k" :: String
        v <- toJSVal (0 :: Int)
        (o <# k) v
        Function _ f1 <- function $ \_ _ _ -> do
          (o <# k) (1 :: Int)
          pure ()
        Function _ f2 <- function $ \_ _ _ -> do
          call f1 f1 ()
          pure ()
        call f2 f2 ()
        o ! k

    it "can be run sequentially in a single call" $
      resultShouldBe "2" $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f1 <- function $ \_ _ _ -> do
          (o <# k) (1 :: Int)
          pure ()
        Function _ f2 <- function $ \_ _ _ -> do
          (o <# k) (2 :: Int)
          pure ()
        let
          jsApi = "(function(f1, f2) {\
                  \  f1();\
                  \  f2();\
                  \})"
        api <- eval jsApi
        call api o [f1, f2]
        o ! k
