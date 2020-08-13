{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
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
    ) where


import Relude.Base ((<))
import Relude.Bool (otherwise)
import Relude.List.NonEmpty
import Relude.List.Reexport
import Relude.Monad (Maybe (..), partitionEithers, Either)
import Relude.Numeric (Int, (-))
import Relude.Function ((.))


-- $setup
-- >>> import Relude

{- | Safer version of 'Relude.Unsafe.!!', returns a Maybe.
get element from list using index value starting from `0`.

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

{- | Partitions a list based on the result of function which produces an Either value. List of all elements producing Left are extracted, in order, to the first element of the output tuple. Similarly, a list of all elements producing Right are extracted to the second element of output.

>>> :{
 foo x
   | even x = Left x
   | otherwise = Right x
 :}

>>> partitionWith foo [1 .. 10]
([2,4,6,8,10],[1,3,5,7,9])
-}
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith = (partitionEithers .) . map
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
