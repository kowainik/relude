{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Utility functions to work with lists.
-}

module Relude.List
  ( module Relude.List.NonEmpty
  , module Relude.List.Reexport
  , (!!?)
  ) where

import Data.Bool (otherwise, (||), (>=))
import Data.List (length, (!!))
import Data.Maybe (Maybe (..))

import Relude.List.NonEmpty
import Relude.List.Reexport
import Relude.Numeric (Int, (<))

-- $setup
-- >>> import Relude
{- | Safer version of '!!', returns a Maybe.
get element from list using index value starting from `0`.

>>> [] !!? 0
Nothing

>>> ["a", "b", "c"] !!? 3
Nothing

>>> ["a", "b", "c"] !!? 2
Just "c"
-}
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)
{-# INLINE (!!?) #-}
