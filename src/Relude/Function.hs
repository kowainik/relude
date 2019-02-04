{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 20016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module reexports very basic and primitive functions and function combinators.
-}

module Relude.Function
       ( module Control.Arrow
       , module Control.Category
       , module Data.Function
       , identity
       ) where

import Control.Arrow ((&&&))
import Control.Category ((<<<), (>>>))
import Data.Function (const, fix, flip, id, on, ($), (&), (.))

-- | Renamed version of 'Prelude.id'.
identity :: a -> a
identity = id
{-# INLINE identity #-}
