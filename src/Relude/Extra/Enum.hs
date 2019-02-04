{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Mini @bounded-enum@ framework inside @relude@.
-}

module Relude.Extra.Enum
       ( universe
       , inverseMap
       , next
       , prec
       , safeToEnum
       ) where

import Relude

import qualified Data.Map.Strict as M

-- $setup
-- >>> :set -XTypeApplications

{- | Returns all values of some 'Bounded' 'Enum' in ascending order.

>>> data TrafficLight = Red | Blue | Green deriving (Show, Enum, Bounded)
>>> universe :: [TrafficLight]
[Red,Blue,Green]
>>> universe :: [Bool]
[False,True]
-}
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

{- | @inverseMap f@ creates a function that is the inverse of a given function
@f@. It does so by constructing 'M.Map' for every value @f a@. The
implementation makes sure that the 'M.Map' is constructed only once and then
shared for every call.

The complexity of reversed mapping though is \(\mathcal{O}(\log n)\).

Usually you want to use 'inverseMap' to inverse 'show' function.

>>> data Color = Red | Green | Blue deriving (Show, Enum, Bounded)
>>> parse = inverseMap show :: String -> Maybe Color
>>> parse "Red"
Just Red
>>> parse "Black"
Nothing
-}
inverseMap :: forall a k . (Bounded a, Enum a, Ord k)
           => (a -> k) -> (k -> Maybe a)
inverseMap f = \k -> M.lookup k dict
  where
    dict :: M.Map k a
    dict = M.fromList $ zip (map f univ) univ

    univ :: [a]
    univ = universe

{- | Like 'succ', but doesn't fail on 'maxBound'. Instead it returns 'minBound'.

>>> next False
True
>>> next True
False
>>> succ True
*** Exception: Prelude.Enum.Bool.succ: bad argument
-}
next  :: (Eq a, Bounded a, Enum a) => a -> a
next e
    | e == maxBound = minBound
    | otherwise     = succ e

{- | Like 'pred', but doesn't fail on 'minBound'. Instead it returns 'maxBound'.

>>> prec False
True
>>> prec True
False
>>> pred False
*** Exception: Prelude.Enum.Bool.pred: bad argument
-}
prec  :: (Eq a, Bounded a, Enum a) => a -> a
prec e
    | e == minBound = maxBound
    | otherwise     = pred e

{- | Returns 'Nothing' if given 'Int' outside range.

>>> safeToEnum @Bool 0
Just False
>>> safeToEnum @Bool 1
Just True
>>> safeToEnum @Bool 2
Nothing
>>> safeToEnum @Bool (-1)
Nothing
-}
safeToEnum :: forall a . (Bounded a, Enum a) => Int -> Maybe a
safeToEnum i = guard (fromEnum @a minBound <= i && i <= fromEnum @a maxBound) *> Just (toEnum i)
