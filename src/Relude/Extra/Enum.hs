{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

Mini @bounded-enum@ framework inside @relude@.

@since 0.1.0
-}

module Relude.Extra.Enum
    ( universe
    , universeNonEmpty
    , inverseMap
    , next
    , prev
    , safeToEnum
    ) where

import Relude
import Relude.Extra.Tuple (fmapToFst)

import qualified Data.Map.Strict as M


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
universeNonEmpty :: forall a . (Bounded a, Enum a, Eq a) => NonEmpty a
universeNonEmpty
    | minBound @a == maxBound = minBound :| []
    | otherwise = minBound :| [succ minBound .. maxBound]
{-# INLINE universeNonEmpty #-}

{- | @inverseMap f@ creates a function that is the inverse of a given function
@f@. It does so by constructing 'M.Map' internally for each value @f a@. The
implementation makes sure that the 'M.Map' is constructed only once and then
shared for every call.

__Memory usage note:__ don't inverse functions that have types like 'Int'
as their result. In this case the created 'M.Map' will have huge size.

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
    dict = M.fromList $ fmapToFst f (universe @a)
{-# INLINE inverseMap #-}

{- | Like 'succ', but doesn't fail on 'maxBound'. Instead it returns 'minBound'.

>>> next False
True
>>> next True
False
>>> succ True
*** Exception: Prelude.Enum.Bool.succ: bad argument

@since 0.1.0
-}
next :: (Eq a, Bounded a, Enum a) => a -> a
next e
    | e == maxBound = minBound
    | otherwise     = succ e
{-# INLINE next #-}

{- | Like 'pred', but doesn't fail on 'minBound'. Instead it returns 'maxBound'.

>>> prev False
True
>>> prev True
False
>>> pred False
*** Exception: Prelude.Enum.Bool.pred: bad argument

@since 0.6.0.0
-}
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev e
    | e == minBound = maxBound
    | otherwise     = pred e
{-# INLINE prev #-}

{- | Returns 'Nothing' if given 'Int' outside range.

>>> safeToEnum @Bool 0
Just False
>>> safeToEnum @Bool 1
Just True
>>> safeToEnum @Bool 2
Nothing
>>> safeToEnum @Bool (-1)
Nothing

@since 0.1.0
-}
safeToEnum :: forall a . (Bounded a, Enum a) => Int -> Maybe a
safeToEnum i = guard (fromEnum @a minBound <= i && i <= fromEnum @a maxBound) $> toEnum i
{-# INLINE safeToEnum #-}
