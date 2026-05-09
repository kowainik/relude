{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.List
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

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
    , permutations
    ) where


import Relude.Base ((<))
import Relude.Bool (otherwise)
import Relude.Function (flip, (.), id)
import Relude.List.NonEmpty
import Relude.List.Reexport
import Relude.Monad (Either, Maybe (..), partitionEithers)
import Relude.Numeric (Int, (-))
import Relude.Foldable (foldr)


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

@since 1.0.0.0
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

@since 1.0.0.0
-}
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = partitionEithers . map f
{-# INLINE partitionWith #-}

{- | The 'permutations' function returns the list of all permutations of the argument.
Unlike its Prelude implementation, thiv version returns NonEmpty.

>>> permutations "abc"
"abc" :| ["bac","cba","bca","cab","acb"]

>>> permutations []
[] :| []

@since 1.1.0.0
-}
permutations            :: [a] -> NonEmpty [a]
permutations xs0        =  xs0 :| perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
{-# INLINE permutations #-}


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
