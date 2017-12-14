module Universum.Function
       ( module Data.Function
       , identity
       ) where

import Data.Function (const, fix, flip, on, ($), (.))

-- | Renamed version of 'Prelude.id'.
identity :: a -> a
identity x = x
