{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

-- | Utility functions to work with lists.

module Universum.List
       ( module Universum.List.Reexport
       , module Universum.List.Safe
       , foldl'
       ) where

import Universum.List.Reexport
import Universum.List.Safe

import GHC.Base (foldr, oneShot, seq, id)

-- Changes the argument order of GHC.List's implementation of foldl' 
foldl' :: forall a b . (a -> b -> b) -> b -> [a] -> b
{-# INLINE foldl' #-}
foldl' k z0 xs = 
    foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> z `seq` fn (k v z))) (id :: b -> b) xs z0
