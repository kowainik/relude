{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Utility functions to work with lists and 'NonEmpty' lists.
-}

module Relude.List
    ( module Relude.List.Reexport
      -- $reexport
    , module Relude.List.NonEmpty
      -- $nonempty
    , (!!?)
    , maybeAt
    , partitionWith
    ) where


import Relude.Base ((<))
import Relude.Bool (otherwise)
import Relude.Function (flip, (.))
import Relude.List.NonEmpty
import Relude.List.Reexport
import Relude.Monad (Either, Maybe (..), partitionEithers)
import Relude.Numeric (Int, (-))


-- $setup
-- >>> import Relude

{- | Safer version of 'Relude.Unsafe.!!', returns a Maybe.

Get element from list using index value starting from `0`.

>>> [] !!? 0
Nothing

>>> ["a", "b", "c"] !!? 3
Nothing

>>> [1, 2, 3] !!? (-1)
Nothing

>>> ["a", "b", "c"] !!? 2
Just "c"

@since 0.6.0.0
-}
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE (!!?) #-}

{- | '!!?' with its arguments flipped.

Get element from list using index value starting from `0`.

>>> maybeAt 0 []
Nothing

>>> maybeAt 3 ["a", "b", "c"]
Nothing

>>> maybeAt (-1) [1, 2, 3]
Nothing

>>> maybeAt 2 ["a", "b", "c"]
Just "c"

-}
maybeAt :: Int -> [a] -> Maybe a
maybeAt = flip (!!?)
{-# INLINE maybeAt #-}

{- | Partitions a list based on the result of function which produces an Either
value. List of all elements producing Left are extracted, in order, to the first
element of the output tuple. Similarly, a list of all elements producing Right
are extracted to the second element of output.

>>> :{
 divideEvenOrShow :: Int -> Either Int String
 divideEvenOrShow n
     | even n = Left $ n `div` 2
     | otherwise = Right $ "Odd: " <> show n
 :}

>>> partitionWith divideEvenOrShow [1 .. 6]
([1,2,3],["Odd: 1","Odd: 3","Odd: 5"])

@since 0.8.0.0
-}
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = partitionEithers . map f
{-# INLINE partitionWith #-}

{- $reexport
Most of the "Data.List" types and function.

Note, that list partial functions (e.g. 'Data.List.head') are not exported from
"Data.List".
Instead @relude@ provides safe functions that work with
'Data.List.NonEmpty.NonEmpty'. You can find them in the
"Relude.List.NonEmpty" module instead.
-}

{- $nonempty
Reexports from "Data.List.NonEmpty" and additional safe functions to work with
list type in terms of 'NonEmpty'.
-}
