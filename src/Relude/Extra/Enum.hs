{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Relude.Extra.Enum
       ( universe
       , makeParser
       , next
       , safeToEnum
       ) where

import Relude

import qualified Data.HashMap.Strict as Map

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
>>> parse = makeParser :: String -> Maybe Color
>>> parse "Red"
Just Red
>>> parse "Black"
Nothing
-}
makeParser :: forall a. (Bounded a, Enum a, Show a) => String -> Maybe a
makeParser = \x -> Map.lookup x dict
    where
        dict = Map.fromList $ zip (map show univ) univ
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
