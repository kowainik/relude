{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Functor
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2022 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Convenient functions to work with 'Functor'.
-}

module Relude.Functor
    ( module Relude.Functor.Reexport
      -- $reexport
    , module Relude.Functor.Fmap
      -- $fmap
    ) where

import Relude.Functor.Fmap
import Relude.Functor.Reexport

{- $reexport
Reexports functionality for 'Functor' and 'Bifunctor' typeclasses.
-}

{- $fmap
Additional useful combinators to work with 'Functor' type class.
For example, '<<$>>' and '??'.
-}
