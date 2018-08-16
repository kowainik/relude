{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functions to work with 'Bool' type.
-}

module Relude.Bool.Reexport
       ( module Control.Monad
       , module Data.Bool
       ) where

import Control.Monad (guard, unless, when)
import Data.Bool (Bool (..), bool, not, otherwise, (&&), (||))
