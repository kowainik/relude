module Universum.Function
       ( module Data.Function
       , identity
       ) where

import Data.Function (const, fix, flip, id, on, ($), (.))

-- | Renamed version of 'Prelude.id'.
identity :: a -> a
identity = id
{-# INLINE identity #-}
