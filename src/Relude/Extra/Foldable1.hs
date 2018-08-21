{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}

module Relude.Extra.Foldable1
    ( Foldable1 (..)
    ) where

import Relude hiding (Product (..), Sum (..))

import Relude.Extra.Newtype (( #. ))

import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import qualified Data.Semigroup as SG

-- | The class of foldable data structures that cannot be empty.

class Foldable t => Foldable1 t where
    {-# MINIMAL foldMap1 #-}

    {- | Map each element of the non-empty structure to a semigroup, and combine the results.

    >>> foldMap1 SG.Sum (1 :| [2, 3, 4])
    Sum {getSum = 10}
    -}
    foldMap1 :: Semigroup m => (a -> m) -> t a -> m

    {- | Combine the elements of a non-empty structure using a semigroup.

    >>> fold1 (1 :| [2, 3, 4 :: SG.Sum Int])
    Sum {getSum = 10}
    >>> fold1 (4 :| [5, 10 :: SG.Product Int])
    Product {getProduct = 200}
    -}
    fold1 :: Semigroup m => t m -> m
    fold1 = foldMap1 id

    {- | Convert a non-empty data structre to a NonEmpty list.

    >>> toNonEmpty (Identity 2)
    2 :| []
    -}
    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = foldMap1 (:|[])

    {- | The first element of a non-empty data structure.

    >>> head1 (1 :| [2, 3, 4])
    1
    -}
    head1 :: t a -> a
    head1 = SG.getFirst #. foldMap1 SG.First

    {- | The last element of a non-empty data structure.

    >>> last1 (1 :| [2, 3, 4])
    4
    -}
    last1 :: t a -> a
    last1 = SG.getLast #. foldMap1 SG.Last

    {- | The largest element of a non-empty data structure.

    >>> maximum1 (32 :| [64, 8, 128, 16])
    128
    -}
    maximum1 :: Ord a => t a -> a
    maximum1 = SG.getMax #. foldMap1 SG.Max

    {- | The smallest elemenet of a non-empty data structure.

    >>> minimum1 (32 :| [64, 8, 128, 16])
    8
    -}
    minimum1 :: Ord a => t a -> a
    minimum1 = SG.getMin #. foldMap1 SG.Min

instance Foldable1 NonEmpty where
    foldMap1 :: Semigroup m => (a -> m) -> NonEmpty a -> m
    foldMap1 f (a :| [])     = f a
    foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)

    toNonEmpty :: NonEmpty a -> NonEmpty a
    toNonEmpty = id

    head1 :: NonEmpty a -> a
    head1 = head

    last1 :: NonEmpty a -> a
    last1 = last

instance Foldable1 Identity where
    foldMap1 :: Semigroup m => (a -> m) -> Identity a -> m
    foldMap1 f = f . coerce

    fold1 :: Semigroup m => Identity m -> m
    fold1 = coerce

    toNonEmpty :: Identity a -> NonEmpty a
    toNonEmpty = (:|[]) . coerce

    head1 :: Identity a -> a
    head1 = coerce

    last1 :: Identity a -> a
    last1 = coerce

    maximum1 :: Ord a => Identity a -> a
    maximum1 = coerce

    minimum1 :: Ord a => Identity a -> a
    minimum1 = coerce

instance Foldable1 ((,) c) where
    foldMap1 :: Semigroup m => (a -> m) -> (c, a) -> m
    foldMap1 f (_, y) = f y

    fold1 :: Semigroup m => (c, m) -> m
    fold1 (_, y) = y

    toNonEmpty :: (c, a) -> NonEmpty a
    toNonEmpty (_, y) = (y :|[])

    head1 :: (c, a) -> a
    head1 (_, y) = y

    last1 :: (c, a) -> a
    last1 (_, y) = y

    maximum1 :: Ord a => (c, a) -> a
    maximum1 (_, y) = y

    minimum1 :: Ord a => (c, a) -> a
    minimum1 (_, y) = y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Compose f g a -> m
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose

    head1 :: Compose f g a -> a
    head1 = head1 . head1 . getCompose

    last1 :: Compose f g a -> a
    last1 = last1 . last1 . getCompose

instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Product f g a -> m
    foldMap1 f (Pair a b) = foldMap1 f a <> foldMap1 f b

instance (Foldable1 f, Foldable1 g) => Foldable1 (Sum f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Sum f g a -> m
    foldMap1 f (InL x) = foldMap1 f x
    foldMap1 f (InR y) = foldMap1 f y

