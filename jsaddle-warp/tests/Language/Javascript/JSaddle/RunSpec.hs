{-# LANGUAGE ScopedTypeVariables #-}
module Language.Javascript.JSaddle.RunSpec where

-- Tests specific to Run.hs, ie non ghcjs

import Prelude hiding ((!!))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import Language.Javascript.JSaddle
import System.Mem
import System.Mem.Weak
import Control.Concurrent

import qualified Language.Javascript.JSaddle.ObjectSpec as ObjectSpec
import qualified Language.Javascript.JSaddle.ValueSpec as ValueSpec

import Test.Hspec

spec :: SpecWith JSContextRef
spec = do
  let
    resultShouldBe res m ctx = do
      result <- runJSM (valToText =<< m) ctx
      result `shouldBe` (T.pack res)

  describe "Callbacks" $ do
    it "Get freed and garbage collected when neither Haskell or JS refers to them" $ \ctx -> do
      result <- flip runJSM ctx $ do
        ref <- liftIO $ newIORef ()
        weakVal <- liftIO $ mkWeakIORef ref (pure ())
        -- Both of these should be GCed
        function $ \_ _ _ -> do
          liftIO $ readIORef ref
          pure ()
        asyncFunction $ \_ _ _ -> do
          liftIO $ readIORef ref
          pure ()
        -- Allocate some more functions, to fill up memory
        replicateM_ 100 $ do
          function' $ \_ _ _ -> do
            toJSVal "callback ran"
          pure ()
        -- Wait till the JS side also does a GC
        do
          doCbAfterGc <- eval doCallbackAfterGCFun
          jsGCDone <- liftIO $ newEmptyMVar
          Function _ fPostGC <- function $ \_ _ _ -> do
            -- liftIO $ putStrLn "GC done"
            liftIO $ putMVar jsGCDone ()
          call doCbAfterGc doCbAfterGc [fPostGC]
          liftIO $ takeMVar jsGCDone
        -- Now confirm the callback was freed
        v <- liftIO $ do
          performGC
          deRefWeak weakVal
        pure $ isNothing v
      result `shouldBe` True

  -- TODO: support JavaScriptException style handling with ghcjs
  let runThis m = m `catchError` (\(JavaScriptException e) -> valToText e)
        `catch` (\(e :: MyException) -> pure (T.pack $ "Caught: " <> show e))
  describe "catchError" $ do
    it "catches JavaScriptException" $
      resultShouldBe "ReferenceError: someUndefinedAPI is not defined" $ runThis $
        eval "someUndefinedAPI()" >> pure (T.pack "should be unreachable")

    it "rethrows other exceptions" $
      resultShouldBe "Caught: ThisException" $ runThis $
        throwM ThisException >> pure (T.pack "should be unreachable")

    -- This is the behaviour of current implementation
    -- It isn't a guaranteed outcome but a most likely one if the Haskell thread does not block
    it "catches exception in a try block with both JavaScriptException and throwM" $
      resultShouldBe "Caught: ThisException" $ runThis $
        eval "someUndefinedAPI()" >> throwM ThisException >> pure (T.pack "should be unreachable")

    it "catches JavaScriptException in a nested catchError" $
      resultShouldBe "handler2" $ runThis $
        (valToText =<< eval ("someUndefinedAPI()"))
          `catchError` (\(JavaScriptException e) -> pure (T.pack "handler2"))

  -- The sync callbacks by default makes use of catchError
  describe "catchError in sync callbacks" $ do
    it "catches JavaScriptException, and rethrows the exception to caller" $
      resultShouldBe "ReferenceError: someUndefinedAPI is not defined" $ runThis $ do
        Function _ f1 <- function $ \_ _ _ -> do
          eval "someUndefinedAPI()"
          pure ()
        call f1 f1 () >> pure (T.pack "should be unreachable")

    it "catches other exceptions, and rethrows the exception to caller" $
      resultShouldBe "ThisException" $ runThis $ do
        Function _ f1 <- function $ \_ _ _ -> do
          throwM ThisException
          pure ()
        call f1 f1 () >> pure (T.pack "should be unreachable")

    -- The 'Non-exhaustive patterns in function' exception happens
    -- immediately on starting callback execution
    it "catches sync callback's Non-exhaustive patterns exception" $
      resultShouldBe "tests/Language/Javascript/JSaddle/RunSpec.hs:(106,37)-(108,17): Non-exhaustive patterns in lambda\n" $ runThis $ do
        Function _ f1 <- function $ \_ _ (_:_) -> do
          eval "someUndefinedAPI()"
          pure ()
        call f1 f1 () >> pure (T.pack "should be unreachable")

    -- The JavaScriptException of the callback may not happen, if the callback thread throws an exception without blocking
    it "catches exception with both JavaScriptException and throwM" $
      resultShouldBe "ThisException" $ runThis $ do
        Function _ f1 <- function $ \_ _ _ -> do
          eval "someUndefinedAPI()"
          throwM ThisException
          pure ()
        call f1 f1 () >> pure (T.pack "should be unreachable")

    it "catches exception with both JavaScriptException and throwM 2" $
      resultShouldBe "ReferenceError: someUndefinedAPI is not defined" $ runThis $ do
        Function _ f1 <- function $ \_ _ _ -> do
          eval "someUndefinedAPI()"
          liftIO $ pure () -- force a sync
          throwM ThisException
          pure ()
        call f1 f1 () >> pure (T.pack "should be unreachable")

    it "sync callback rethrows exception to the JS caller" $
      resultShouldBe "caught exception" $ runThis $ do
        Function _ f1 <- function $ \_ _ _ -> do
          eval "someUndefinedAPI()"
          pure ()
        let
          jsApi = "(function(callback) {\
                  \  try {\
                  \    callback();\
                  \    return 'exception did not happen';\
                  \  } catch (e) { return 'caught exception'; }\
                  \})"
        api <- eval jsApi
        valToText =<< call api f1 ()

    it "catches JavaScriptException in nested callbacks" $
      resultShouldBe "0" $ runThis $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f2 <- function $ \_ _ _ -> do
          eval "someUndefinedAPI()"
          pure ()
        Function _ f1 <- function $ \_ _ _ -> do
          call f2 f2 ()
          (o <# k) (1 :: Int)
          pure ()
        call f1 f1 ()
          `catchError` (\(JavaScriptException e) -> pure e)
        valToText =<< o ! k

    it "catches throwM exceptions in nested callbacks 1" $
      resultShouldBe "0" $ runThis $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f2 <- function $ \_ _ _ -> do
          throwM ThisException
          pure ()
        Function _ f1 <- function $ \_ _ _ -> do
          call f2 f2 ()
          (o <# k) (1 :: Int)
          pure ()
        call f1 f1 ()
          `catchError` (\(JavaScriptException e) -> pure e)
        valToText =<< o ! k

    it "catches JavaScriptException in nested callbacks, ensuring sequential execution" $
      resultShouldBe "2" $ runThis $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f2 <- function $ \_ _ _ -> do
          (o <# k) (2 :: Int)
          pure ()
        Function _ f1 <- function $ \_ _ _ -> do
          (o <# k) (1 :: Int)
          call f2 f2 ()
          eval "someUndefinedAPI()"
          pure ()
        call f1 f1 ()
          `catchError` (\(JavaScriptException e) -> pure e)
        valToText =<< o ! k

    it "catches throwM exceptions in nested callbacks, ensuring sequential execution" $
      resultShouldBe "2" $ runThis $ do
        o <- create
        let k = "k" :: String
        (o <# k) (0 :: Int)
        Function _ f2 <- function $ \_ _ _ -> do
          (o <# k) (2 :: Int)
          pure ()
        Function _ f1 <- function $ \_ _ _ -> do
          (o <# k) (1 :: Int)
          call f2 f2 ()
          throwM ThisException
          pure ()
        call f1 f1 ()
          `catchError` (\(JavaScriptException e) -> pure e)
        valToText =<< o ! k

data MyException = ThisException | ThatException
    deriving Show

instance Exception MyException

-- This triggers a GC on the JS side
doCallbackAfterGCFun = "\
    \function doCallbackAfterGC(callback){\n\
    \  let arrayCollected = false;\n\
    \  const registry = new FinalizationRegistry(() => {\n\
    \    arrayCollected = true;\n\
    \  });\n\
    \\n\
    \  (function allocateMemory() {\n\
    \    var a = Array.from({ length: 50000 }, () => () => {});\n\
    \    registry.register(a);\n\
    \    if (arrayCollected) {\n\
    \      callback();\n\
    \      return;\n\
    \    };\n\
    \    setTimeout(allocateMemory);\n\
    \  })();\n\
    \}\n\
    \doCallbackAfterGC\n";
