{-# LANGUAGE TypeApplications #-}

{- |
Copyright:  (c) 2011-2015 Edward Kmett
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Relude.Extra.Foldable1
       ( Foldable1 (..)
       , foldl1'
       ) where

import Relude hiding (Product (..), Sum (..))
import Relude.Extra.Newtype (( #. ))

import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))

import qualified Data.Semigroup as SG


{- | The class of foldable data structures that cannot be empty.
-}
class Foldable f => Foldable1 f where
    {-# MINIMAL foldMap1 #-}

    {- | Map each element of the non-empty structure to a semigroup, and combine the results.

    >>> foldMap1 SG.Sum (1 :| [2, 3, 4])
    Sum {getSum = 10}
    -}
    foldMap1 :: Semigroup m => (a -> m) -> f a -> m

    {- | Combine the elements of a non-empty structure using a semigroup.

    >>> fold1 (1 :| [2, 3, 4 :: SG.Sum Int])
    Sum {getSum = 10}
    >>> fold1 (4 :| [5, 10 :: SG.Product Int])
    Product {getProduct = 200}
    -}
    fold1 :: Semigroup m => f m -> m
    fold1 = foldMap1 id

    {- | Convert a non-empty data structre to a NonEmpty list.

    >>> toNonEmpty (Identity 2)
    2 :| []
    -}
    toNonEmpty :: f a -> NonEmpty a
    toNonEmpty = foldMap1 (:|[])

    {- | The first element of a non-empty data structure.

    >>> head1 (1 :| [2, 3, 4])
    1
    -}
    head1 :: f a -> a
    head1 = SG.getFirst #. foldMap1 SG.First

    {- | The last element of a non-empty data structure.

    >>> last1 (1 :| [2, 3, 4])
    4
    -}
    last1 :: f a -> a
    last1 = SG.getLast #. foldMap1 SG.Last

    {- | The largest element of a non-empty data structure.

    >>> maximum1 (32 :| [64, 8, 128, 16])
    128
    -}
    maximum1 :: Ord a => f a -> a
    maximum1 = SG.getMax #. foldMap1 SG.Max

    {- | The smallest elemenet of a non-empty data structure.

    >>> minimum1 (32 :| [64, 8, 128, 16])
    8
    -}
    minimum1 :: Ord a => f a -> a
    minimum1 = SG.getMin #. foldMap1 SG.Min

instance Foldable1 NonEmpty where
    fold1 :: Semigroup m => NonEmpty m -> m
    fold1 = sconcat
    {-# INLINE fold1 #-}

    foldMap1 :: forall m a . Semigroup m => (a -> m) -> NonEmpty a -> m
    foldMap1 f (a :| as) = foldr go f as a
      where
        go :: a -> (a -> m) -> a -> m
        go b g x = f x <> g b
    {-# INLINE foldMap1 #-}

    toNonEmpty :: NonEmpty a -> NonEmpty a
    toNonEmpty = id
    {-# INLINE toNonEmpty #-}

    head1, last1 :: NonEmpty a -> a
    head1 = head
    last1 = last
    {-# INLINE head1 #-}
    {-# INLINE last1 #-}

    maximum1, minimum1 :: Ord a => NonEmpty a -> a
    maximum1 = foldl1' max
    minimum1 = foldl1' min
    {-# INLINE maximum1 #-}
    {-# INLINE minimum1 #-}

instance Foldable1 Identity where
    foldMap1 :: Semigroup m => (a -> m) -> Identity a -> m
    foldMap1 = coerce
    {-# INLINE foldMap1 #-}

    fold1 :: Semigroup m => Identity m -> m
    fold1 = coerce
    {-# INLINE fold1 #-}

    toNonEmpty :: Identity a -> NonEmpty a
    toNonEmpty = (:|[]) . coerce
    {-# INLINE toNonEmpty #-}

    head1 :: Identity a -> a
    head1 = coerce
    {-# INLINE head1 #-}

    last1 :: Identity a -> a
    last1 = coerce
    {-# INLINE last1 #-}

    maximum1 :: Ord a => Identity a -> a
    maximum1 = coerce
    {-# INLINE maximum1 #-}

    minimum1 :: Ord a => Identity a -> a
    minimum1 = coerce
    {-# INLINE minimum1 #-}

instance Foldable1 ((,) c) where
    foldMap1 :: Semigroup m => (a -> m) -> (c, a) -> m
    foldMap1 f = f . snd
    {-# INLINE foldMap1 #-}

    fold1 :: Semigroup m => (c, m) -> m
    fold1 = snd
    {-# INLINE fold1 #-}

    toNonEmpty :: (c, a) -> NonEmpty a
    toNonEmpty (_, y) = (y :| [])
    {-# INLINE toNonEmpty #-}

    head1, last1 :: (c, a) -> a
    head1 = snd
    last1 = snd
    {-# INLINE head1 #-}
    {-# INLINE last1 #-}

    maximum1, minimum1 :: Ord a => (c, a) -> a
    maximum1 = snd
    minimum1 = snd
    {-# INLINE maximum1 #-}
    {-# INLINE minimum1 #-}

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Compose f g a -> m
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose
    {-# INLINE foldMap1 #-}

    head1 :: Compose f g a -> a
    head1 = head1 . head1 . getCompose
    {-# INLINE head1 #-}

    last1 :: Compose f g a -> a
    last1 = last1 . last1 . getCompose
    {-# INLINE last1 #-}

instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Product f g a -> m
    foldMap1 f (Pair a b) = foldMap1 f a <> foldMap1 f b
    {-# INLINE foldMap1 #-}

instance (Foldable1 f, Foldable1 g) => Foldable1 (Sum f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Sum f g a -> m
    foldMap1 f (InL x) = foldMap1 f x
    foldMap1 f (InR y) = foldMap1 f y
    {-# INLINE foldMap1 #-}

{- | Strictly folds non-empty structure with given function @f@:

@
foldl1' f [x0, x1, x2 ...] = f (f x0 x1) x2 ...
@

>>> foldl1' (++) ([1,2] :| [[3,4], [5,6]])
[1,2,3,4,5,6]
-}
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' _ (x :| [])     = x
foldl1' f (x :| (y:ys)) = foldl' f (f x y) ys
{-# INLINE foldl1' #-}
