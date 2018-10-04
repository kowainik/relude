{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Copyright:  (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains useful functions to work with Types.
-}

module Relude.Extra.Type
       ( typeName
       ) where

import Relude

#if ( __GLASGOW_HASKELL__ >= 822 )
import Type.Reflection (Typeable, typeRep)
#else
import Data.Typeable (Typeable, typeRep)
#endif

{- | Gets a string representation of a type.

Note: This must be used with -XTypeApplications

>>> typeName @()
"()"
>>> typeName @Int
"Int"
>>> typeName @String
"[Char]"
>>> typeName @[Maybe Int]
"[Maybe Int]"
-}
typeName :: forall a. Typeable a => Text
#if ( __GLASGOW_HASKELL__ >= 822 )
typeName = show (typeRep @a)
#else
typeName = show (typeRep (Proxy @a))
#endif
{-# INLINE typeName #-}