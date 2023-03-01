{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module                  : Relude.Foldable.Fold
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Fixes and additions to 'Foldable'. Specifically:

* Space-leak free 'sum' and 'product'
* 'elem' and 'notElem' are forbidden for 'Set' and 'HashSet'
* Additional combinators for common idioms
-}

module Relude.Foldable.Fold
    ( flipfoldl'
    , asumMap
    , foldMapA
    , foldMapM

    , sum
    , product

    , elem
    , notElem

      -- * Monadic functions
    , allM
    , anyM
    , andM
    , orM
    ) where

import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

import Relude.Applicative (Alternative, Applicative (..), pure)
import Relude.Base (Constraint, Eq, IO, Type, coerce, ($!))
import Relude.Bool (Bool (..), (&&^), (||^))
import Relude.Container.Reexport (HashSet, Set)
import Relude.Foldable.Reexport (Foldable (..))
import Relude.Function (flip, (.))
import Relude.Monad.Reexport (Monad (..))
import Relude.Monoid (Alt (..), Ap (..), Monoid (..), Semigroup)
import Relude.Numeric (Num (..))

import qualified Data.Foldable as F


-- $setup
-- >>> import Relude
-- >>> import qualified Data.HashMap.Strict as HashMap

{- | Similar to 'foldl'' but takes a function with its arguments flipped.

>>> flipfoldl' (/) 5 [2,3] :: Rational
15 % 2

This function can be useful for constructing containers from lists.
-}
flipfoldl' :: Foldable f => (a -> b -> b) -> b -> f a -> b
flipfoldl' f = foldl' (flip f)
{-# INLINE flipfoldl' #-}

{- | Alternative version of 'Relude.asum' that takes a function to map over.

>>> asumMap (\x -> if x > 2 then Just x else Nothing) [1..4]
Just 3

@since 0.4.0
-}
asumMap :: forall b m f a . (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap = coerce (foldMap :: (a -> Alt m b) -> f a -> Alt m b)
{-# INLINE asumMap #-}

{- | Polymorphic version of the 'Relude.concatMapA' function.

>>> foldMapA @[Int] (Just . replicate 3) [1..3]
Just [1,1,1,2,2,2,3,3,3]

@since 0.1.0
-}
foldMapA
    :: forall b m f a . (Semigroup b, Monoid b, Applicative m, Foldable f)
    => (a -> m b)
    -> f a
    -> m b
foldMapA = coerce (foldMap :: (a -> Ap m b) -> f a -> Ap m b)
{-# INLINE foldMapA #-}

{- | Polymorphic version of the 'Relude.concatMapM' function.

>>> foldMapM @[Int] (Just . replicate 3) [1..3]
Just [1,1,1,2,2,2,3,3,3]

@since 0.1.0
-}
foldMapM :: forall b m f a . (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty
  where
    step x r z = f x >>= \y -> r $! z `mappend` y
{-# INLINE foldMapM #-}

{- | Stricter version of 'F.sum'.

>>> sum [1..10]
55
-}
sum :: forall a f . (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0
{-# INLINE sum #-}

{- | Stricter version of 'F.product'.

>>> product [1..10]
3628800
-}
product :: forall a f . (Foldable f, Num a) => f a -> a
product = foldl' (*) 1
{-# INLINE product #-}

{- | Like 'F.elem' but doesn't work on 'Set' and 'HashSet' for performance reasons.

>>> elem 'x' ("abc" :: String)
False
>>> elem False (one True :: Set Bool)
...
... Do not use 'elem' and 'notElem' methods from 'Foldable' on Set
      Suggestions:
          Instead of
              elem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              member :: Ord a => a -> Set a -> Bool
...
          Instead of
              notElem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              not . member
...
-}
elem :: (Foldable f, DisallowElem f, Eq a) => a -> f a -> Bool
elem = F.elem
{-# INLINE elem #-}

{- | Like 'F.notElem' but doesn't work on 'Set' and 'HashSet' for performance reasons.

>>> notElem 'x' ("abc" :: String)
True
>>> notElem False (one True :: Set Bool)
...
... Do not use 'elem' and 'notElem' methods from 'Foldable' on Set
      Suggestions:
          Instead of
              elem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              member :: Ord a => a -> Set a -> Bool
...
          Instead of
              notElem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              not . member
...
-}
notElem :: (Foldable f, DisallowElem f, Eq a) => a -> f a -> Bool
notElem = F.notElem
{-# INLINE notElem #-}

{- | Monadic version of 'F.and'.

>>> andM [Just True, Just False]
Just False
>>> andM [Just True]
Just True
>>> andM [Just True, Just False, Nothing]
Just False
>>> andM [Just True, Nothing]
Nothing
>>> andM [putTextLn "1" >> pure True, putTextLn "2" >> pure False, putTextLn "3" >> pure True]
1
2
False
-}
andM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
andM = foldr (&&^) (pure True)
{-# INLINE andM #-}
{-# SPECIALIZE andM :: [IO Bool] -> IO Bool #-}

{- | Monadic version of 'F.or'.

>>> orM [Just True, Just False]
Just True
>>> orM [Just True, Nothing]
Just True
>>> orM [Nothing, Just True]
Nothing
-}
orM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
orM = foldr (||^) (pure False)
{-# INLINE orM #-}
{-# SPECIALIZE orM  :: [IO Bool] -> IO Bool #-}

{- | Monadic version of 'F.all'.

>>> allM (readMaybe >=> pure . even) ["6", "10"]
Just True
>>> allM (readMaybe >=> pure . even) ["5", "aba"]
Just False
>>> allM (readMaybe >=> pure . even) ["aba", "10"]
Nothing
-}
allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = foldr ((&&^) . p) (pure True)
{-# INLINE allM #-}
{-# SPECIALIZE allM :: (a -> IO Bool) -> [a] -> IO Bool #-}

{- | Monadic  version of 'F.any'.

>>> anyM (readMaybe >=> pure . even) ["5", "10"]
Just True
>>> anyM (readMaybe >=> pure . even) ["10", "aba"]
Just True
>>> anyM (readMaybe >=> pure . even) ["aba", "10"]
Nothing
-}
anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = foldr ((||^) . p) (pure False)
{-# INLINE anyM #-}
{-# SPECIALIZE anyM :: (a -> IO Bool) -> [a] -> IO Bool #-}

----------------------------------------------------------------------------
-- Type level tricks
----------------------------------------------------------------------------

{- | Type family that produces compile-time errors when 'elem' and 'notElem'
functions are used with 'Set' and 'HashSet'.
-}
type family DisallowElem (f :: Type -> Type) :: Constraint where
    DisallowElem     Set = TypeError (ElemErrorMessage Set SetMemberType)
    DisallowElem HashSet = TypeError (ElemErrorMessage HashSet HashSetMemberType)
    DisallowElem f       = ()


type family ElemErrorMessage (t :: k) (msg :: Symbol) :: ErrorMessage where
    ElemErrorMessage t msg =
              'Text "Do not use 'elem' and 'notElem' methods from 'Foldable' on " ':<>: 'ShowType t
        ':$$: 'Text "Suggestions:"
        ':$$: 'Text "    Instead of"
        ':$$: 'Text "        elem :: (Foldable t, Eq a) => a -> t a -> Bool"
        ':$$: 'Text "    use"
        ':$$: 'Text "        member :: " ':<>: 'Text msg
        ':$$: 'Text ""
        ':$$: 'Text "    Instead of"
        ':$$: 'Text "        notElem :: (Foldable t, Eq a) => a -> t a -> Bool"
        ':$$: 'Text "    use"
        ':$$: 'Text "        not . member"
        ':$$: 'Text ""

type SetMemberType = "Ord a => a -> Set a -> Bool"
type HashSetMemberType = "(Eq a, Hashable a) => a -> HashSet a -> Bool"
