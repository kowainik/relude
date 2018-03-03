-- | This module reexports functions to work with 'Bool' type.

module Universum.Bool.Reexport
       ( module Control.Monad
       , module Data.Bool
       ) where

import Control.Monad (guard, unless, when)
import Data.Bool (Bool (..), bool, not, otherwise, (&&), (||))
