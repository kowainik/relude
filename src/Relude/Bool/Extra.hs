{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Bool.Extra
Copyright               : (c) 2025 drlkf
                          (c) 2025 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Convenient functions to work with predicates.
-}

module Relude.Bool.Extra
    ( (.&&)
    , (.||)
    ) where

import Relude.Bool.Reexport (Bool, (&&), (||))

{- | Predicate @and@ combinator.

Combine two predicate functions into one with @and@ boolean logic.

>>> even .&& (> 0) $ 2
True

>>> odd .&& (> 0) $ 2
False

>>> even .&& (< 0) $ 2
False

-}
infixr 3 .&&
(.&&)
  :: (a -> Bool)
  -> (a -> Bool)
  -> (a -> Bool)
(.&&) p1 p2 a = p1 a && p2 a
{-# INLINE (.&&) #-}

{- | Predicate @or@ combinator.

Combine two predicate functions into one with @or@ boolean logic.
Lazy in the second argument.

>>> even .|| (> 0) $ 2
True

>>> even .|| (< 0) $ 2
True

>>> even .|| error "impossible" $ 2
False

-}
infixr 2 .||
(.||)
  :: (a -> Bool)
  -> (a -> Bool)
  -> (a -> Bool)
(.||) p1 p2 a = p1 a || p2 a
{-# INLINE (.||) #-}
