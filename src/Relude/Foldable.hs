{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Foldable
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module provides 'Foldable' and 'Traversable' related types and functions.
-}

module Relude.Foldable
    ( module Relude.Foldable.Reexport
      -- $reexport
    , module Relude.Foldable.Fold
      -- $fold
    ) where

import Relude.Foldable.Fold
import Relude.Foldable.Reexport

{- $reexport
Reexports types and functions from "Data.Foldable" and "Data.Traversable".
-}

{- $fold
Type safe versions and additional functions to 'Foldable'.
-}
