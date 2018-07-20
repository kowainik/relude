{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module reexports very basic and primitive functions and function combinators.

module Relude.Function
       ( module Data.Function
       , identity
       ) where

import Data.Function (const, fix, flip, id, on, ($), (.))

-- | Renamed version of 'Prelude.id'.
identity :: a -> a
identity = id
{-# INLINE identity #-}
