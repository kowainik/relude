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

>>> ["a", "b", "c"] !!? 2
Just "c"
-}
infixl 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0     = Nothing
  | otherwise = f i xs
  where f 0 (x:_)  = Just x
        f j (_:ys) = f (j - 1) ys
        f _ []     = Nothing
{-# INLINE (!!?) #-}
