{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
#ifdef ghcjs_HOST_OS
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-dodgy-imports #-}
#endif
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Monad
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | JSM monad keeps track of the JavaScript context
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Monad (
  -- * Types
    JSM(..)
  , MonadJSM
  , liftJSM
  , askJSM
  , runJSM
  , runJSMCheap
  , syncPoint
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (catch, bracket)
import Language.Javascript.JSaddle.Types (JSM(..), MonadJSM, liftJSM, askJSM, JSContextRef, runJSM, runJSMCheap, waitForSync)
import Control.Concurrent.MVar (MVar, newMVar)
import System.IO.Unsafe (unsafePerformIO)

syncPoint :: MonadJSM m => m ()
syncPoint = liftJSM $ waitForSync
