{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Relude.Extra.Enum
       ( universe
       , inverseMap
       , next
       , safeToEnum
       ) where

import Relude

import qualified Data.HashMap.Strict as HM

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

{- | Create a function that parses values of type @a@

>>> data Color = Red | Green | Blue deriving (Show, Enum, Bounded)
>>> parse = inverseMap :: (Color -> String) -> String -> Maybe Color
>>> parse show "Red"
Just Red
>>> parse show "Black"
Nothing
-}
inverseMap :: forall a k. (Bounded a, Enum a, Eq k, Hashable k)
           => (a -> k)
           -> k
           -> Maybe a
inverseMap f x = HM.lookup x dict
    where
        dict :: HM.HashMap k a
        dict = HM.fromList $ zip (map f univ) univ

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
