{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHCJS.Marshal.Pure ( PFromJSVal(..)
                          , PToJSVal(..)
                          ) where

import           GHCJS.Types
import           GHCJS.Foreign.Internal (jsFalse, jsTrue, jsNull)
import           GHCJS.Marshal.Internal
import           GHCJS.Prim.Internal (primToJSVal, PrimVal(..))

import Data.Text (Text)

instance PFromJSVal JSVal where pFromJSVal = id
                                {-# INLINE pFromJSVal #-}
instance PFromJSVal ()    where pFromJSVal _ = ()
                                {-# INLINE pFromJSVal #-}

instance PToJSVal JSVal     where pToJSVal = id
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Bool      where pToJSVal True     = jsTrue
                                  pToJSVal False    = jsFalse
                                  {-# INLINE pToJSVal #-}

instance PToJSVal a => PToJSVal (Maybe a) where
    pToJSVal Nothing  = jsNull
    pToJSVal (Just a) = pToJSVal a
    {-# INLINE pToJSVal #-}

instance PToJSVal Text where
  pToJSVal s = primToJSVal $ PrimVal_String s

instance PToJSVal Int where
  pToJSVal i = primToJSVal $ PrimVal_Number $ fromIntegral i
