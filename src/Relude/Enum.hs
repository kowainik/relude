{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Reexports 'Enum' related typeclasses and functions. Also introduces a few useful
helpers to work with Enums.

__Note:__ 'universe', 'universeNonEmpty' and 'inverseMap' were previously in the
extra modules, but due to their benefit in different use cases. If you imported
@Relude.Extra.Enum@ module, you can remove it now, as these functions are
reexported in the main "Relude" module.

@since 1.0.0.0
-}

module Relude.Enum
    ( -- * Useful combinators for Enums
      universe
    , universeNonEmpty
    , inverseMap
      -- * Base reexports
    , module GHC.Enum
    ) where

import GHC.Enum (Bounded (..), Enum (..), boundedEnumFrom, boundedEnumFromThen)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List (drop)
import Data.Maybe (Maybe (..))

import Relude.Base (Ord (..))
import Relude.Extra.Tuple (fmapToFst)

import qualified Data.Map.Strict as M


-- $setup
-- >>> import Relude

{- | Returns all values of some 'Bounded' 'Enum' in ascending order.

>>> universe :: [Bool]
[False,True]

>>> universe @Ordering
[LT,EQ,GT]

>>> data TrafficLight = Red | Blue | Green deriving (Show, Enum, Bounded)
>>> universe :: [TrafficLight]
[Red,Blue,Green]

>>> data Singleton = Singleton deriving (Show, Enum, Bounded)
>>> universe @Singleton
[Singleton]

@since 0.1.0
-}
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]
{-# INLINE universe #-}

{- | Like 'universe', but returns 'NonEmpty' list of some enumeration

>>> universeNonEmpty :: NonEmpty Bool
False :| [True]

>>> universeNonEmpty @Ordering
LT :| [EQ,GT]

>>> data TrafficLight = Red | Blue | Green deriving (Show, Eq, Enum, Bounded)
>>> universeNonEmpty :: NonEmpty TrafficLight
Red :| [Blue,Green]

>>> data Singleton = Singleton deriving (Show, Eq, Enum, Bounded)
>>> universeNonEmpty @Singleton
Singleton :| []

@since 0.7.0.0
-}
universeNonEmpty :: forall a . (Bounded a, Enum a) => NonEmpty a
universeNonEmpty = minBound :| drop 1 universe
{-# INLINE universeNonEmpty #-}

{- | @inverseMap f@ creates a function that is the inverse of a given function
@f@. It does so by constructing 'M.Map' internally for each value @f a@. The
implementation makes sure that the 'M.Map' is constructed only once and then
shared for every call.

__Memory usage note:__ don't inverse functions that have types like 'Int'
as their input. In this case the created 'M.Map' will have huge size.

The complexity of reversed mapping is \(\mathcal{O}(\log n)\).

__Performance note:__ make sure to specialize monomorphic type of your functions
that use 'inverseMap' to avoid 'M.Map' reconstruction.

One of the common 'inverseMap' use-case is inverting the 'show' or a 'show'-like
function.

>>> data Color = Red | Green | Blue deriving (Show, Enum, Bounded)
>>> parse = inverseMap show :: String -> Maybe Color
>>> parse "Red"
Just Red
>>> parse "Black"
Nothing

__Correctness note:__ 'inverseMap' expects /injective function/ as its argument,
i.e. the function must map distinct arguments to distinct values.

Typical usage of this function looks like this:

@
__data__ GhcVer
    = Ghc802
    | Ghc822
    | Ghc844
    | Ghc865
    | Ghc881
    __deriving__ ('Eq', 'Ord', 'Show', 'Enum', 'Bounded')

showGhcVer :: GhcVer -> 'Text'
showGhcVer = \\__case__
    Ghc802 -> "8.0.2"
    Ghc822 -> "8.2.2"
    Ghc844 -> "8.4.4"
    Ghc865 -> "8.6.5"
    Ghc881 -> "8.8.1"

parseGhcVer :: 'Text' -> 'Maybe' GhcVer
parseGhcVer = 'inverseMap' showGhcVer
@

@since 0.1.1
-}
inverseMap
    :: forall a k .
       (Bounded a, Enum a, Ord k)
    => (a -> k)
    -> (k -> Maybe a)
inverseMap f = \k -> M.lookup k dict
  where
    dict :: M.Map k a
    dict = M.fromList (fmapToFst f (universe @a))
{-# INLINE inverseMap #-}
