{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

This module exports all container-related stuff.
-}

module Relude.Container
    ( module Relude.Container.One
      -- $one
    , module Relude.Container.Reexport
      -- $reexport
    ) where

import Relude.Container.One
import Relude.Container.Reexport

{- $one
'One' is a typeclass for creating structures from a singleton element.
This module provides many useful instances of 'One' for common containers
as well.
-}

{- $reexport
Reexports container-related functions, typeclasses and data types from
the following packages:

* [@base@](https://hackage.haskell.org/package/base)
* [@containers@](https://hackage.haskell.org/package/containers)
* [@unordered-containers@](https://hackage.haskell.org/package/unordered-containers)
-}
