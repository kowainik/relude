{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Bool.Reexport
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Reexports functions to work with 'Bool' type.
-}

module Relude.Bool.Reexport
    ( -- * "Data.Bool" reexports
      Bool (..)
    , bool
    , not
    , otherwise
    , (&&)
    , (||)
      -- * "Control.Monad" reexports
    , guard
    , when
    , unless
    ) where

import Control.Monad (guard, unless, when)
import Data.Bool (Bool (..), bool, not, otherwise, (&&), (||))
