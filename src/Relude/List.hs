{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Utility functions to work with lists.
-}

module Relude.List
    ( module Relude.List.NonEmpty
    , module Relude.List.Reexport
    , (!!?)
    ) where


import Relude.Base ((<))
import Relude.Bool (otherwise)
import Relude.List.NonEmpty
import Relude.List.Reexport
import Relude.Monad (Maybe (..))
import Relude.Numeric (Int, (-))


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
