module Language.Javascript.JSaddle.RunSpec where

-- Tests specific to Run.hs, ie non ghcjs

import Prelude hiding ((!!))
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
