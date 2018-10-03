{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
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
import Type.Reflection (Typeable, typeRep)

{- | Gets a string representation of a type.

Note: This must be used with -XTypeApplications

>>> typeName @()
"()"
>>> typeName @Int
"Int"
>>> typeName @String
"[Char]"
-}
typeName :: forall a. Typeable a => Text
typeName = show (typeRep @a)
