{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Fixes and additions to 'Foldable'.
-}

module Relude.Foldable.Fold
       ( flipfoldl'
       , foldMapA
       , asumMap
       , foldMapM
       , sum
       , product

       , elem
       , notElem

       , allM
       , anyM
       , andM
       , orM

         -- * Internals
       , DisallowElem
       , ElemErrorMessage
       ) where

import GHC.TypeLits (ErrorMessage (..), TypeError)

import Relude.Applicative (Alternative, Applicative (..), pure)
import Relude.Base (Constraint, Eq, IO, Type, ($!))
import Relude.Bool (Bool (..))
import Relude.Container.Reexport (HashSet, Set)
import Relude.Foldable.Reexport (Foldable (..))
import Relude.Function (flip, (.))
import Relude.Functor ((<$>))
import Relude.Monad.Reexport (Monad (..))
import Relude.Monoid (Alt (..), Monoid (..))
import Relude.Numeric (Num (..))

import qualified Data.Foldable as F

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Relude
-- >>> import qualified Data.HashMap.Strict as HashMap

{- | Similar to 'foldl'' but takes a function with its arguments flipped.

>>> flipfoldl' (/) 5 [2,3] :: Rational
15 % 2
-}
flipfoldl' :: Foldable f => (a -> b -> b) -> b -> f a -> b
flipfoldl' f = foldl' (flip f)
{-# INLINE flipfoldl' #-}

{- | Polymorphic version of @concatMapA@ function.

>>> foldMapA @[Int] (Just . replicate 3) [1..3]
Just [1,1,1,2,2,2,3,3,3]
-}
foldMapA :: forall b m f a . (Monoid b, Applicative m, Foldable f) => (a -> m b) -> f a -> m b
foldMapA f = foldr step (pure mempty)
  where
    step a mb = mappend <$> f a <*> mb
{-# INLINE foldMapA #-}

{- | Alternative version of @asum@
>>> asumMap (\x -> if x > 2 then Just x else Nothing) [1..3]
Just 3
-}
asumMap :: (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap f = getAlt . foldMap (Alt . f)
{-# INLINE asumMap #-}

{- | Polymorphic version of @concatMapM@ function.

>>> foldMapM @[Int] (Just . replicate 3) [1..3]
Just [1,1,1,2,2,2,3,3,3]
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
    • Do not use 'elem' and 'notElem' methods from 'Foldable' on Set
      Suggestions:
          Instead of
              elem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              member :: ??? -- TODO
...
          Instead of
              notElem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              notMember :: ??? -- TODO
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
    • Do not use 'elem' and 'notElem' methods from 'Foldable' on Set
      Suggestions:
          Instead of
              elem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              member :: ??? -- TODO
...
          Instead of
              notElem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              notMember :: ??? -- TODO
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
andM = go . toList
  where
    go []     = pure True
    go (p:ps) = do
        q <- p
        if q then go ps else pure False

{- | Monadic version of 'F.or'.

>>> orM [Just True, Just False]
Just True
>>> orM [Just True, Nothing]
Just True
>>> orM [Nothing, Just True]
Nothing
-}
orM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
orM = go . toList
  where
    go []     = pure False
    go (p:ps) = do
        q <- p
        if q then pure True else go ps

{- | Monadic version of 'F.all'.

>>> allM (readMaybe >=> pure . even) ["6", "10"]
Just True
>>> allM (readMaybe >=> pure . even) ["5", "aba"]
Just False
>>> allM (readMaybe >=> pure . even) ["aba", "10"]
Nothing
-}
allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = go . toList
  where
    go []     = pure True
    go (x:xs) = do
        q <- p x
        if q then go xs else pure False

{- | Monadic  version of 'F.any'.

>>> anyM (readMaybe >=> pure . even) ["5", "10"]
Just True
>>> anyM (readMaybe >=> pure . even) ["10", "aba"]
Just True
>>> anyM (readMaybe >=> pure . even) ["aba", "10"]
Nothing
-}
anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = go . toList
  where
    go []     = pure False
    go (x:xs) = do
        q <- p x
        if q then pure True else go xs

{-# SPECIALIZE andM :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE orM  :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE anyM :: (a -> IO Bool) -> [a] -> IO Bool #-}
{-# SPECIALIZE allM :: (a -> IO Bool) -> [a] -> IO Bool #-}

----------------------------------------------------------------------------
-- Type level tricks
----------------------------------------------------------------------------

type family DisallowElem (f :: Type -> Type) :: Constraint where
    DisallowElem     Set = TypeError (ElemErrorMessage Set)
    DisallowElem HashSet = TypeError (ElemErrorMessage HashSet)
    DisallowElem f       = ()

type family ElemErrorMessage (t :: k) :: ErrorMessage where
    ElemErrorMessage t =
             Text "Do not use 'elem' and 'notElem' methods from 'Foldable' on " :<>: ShowType t
        :$$: Text "Suggestions:"
        :$$: Text "    Instead of"
        :$$: Text "        elem :: (Foldable t, Eq a) => a -> t a -> Bool"
        :$$: Text "    use"
        :$$: Text "        member :: ??? -- TODO"
        :$$: Text ""
        :$$: Text "    Instead of"
        :$$: Text "        notElem :: (Foldable t, Eq a) => a -> t a -> Bool"
        :$$: Text "    use"
        :$$: Text "        notMember :: ??? -- TODO"
        :$$: Text ""
