module Relude.Extra.Foldable1
    ( Foldable1 (..)
    , traverse1_
    , for1_
    ) where

import Relude.Applicative (Applicative, pass)
import Relude.Base (Ord)
import Relude.Foldable.Reexport (Foldable (foldr))
import Relude.Function (flip, id, (.))
import Relude.Functor.Reexport (Compose (..), Identity (..))
import Relude.Monoid (Semigroup, (<>))

import Control.Applicative ((*>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (First (..), Last (..), Max (..), Min (..))

class Foldable t => Foldable1 t where
    foldMap1   :: Semigroup m => (a -> m) -> t a -> m

    fold1 :: Semigroup m => t m -> m
    fold1 = foldMap1 id

    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = foldMap1 (:|[])

    head1 :: t a -> a
    head1 = getFirst . foldMap1 First

    last1 :: t a -> a
    last1 = getLast . foldMap1 Last

    maximum1 :: Ord a => t a -> a
    maximum1 = getMax . foldMap1 Max

    minimum1 :: Ord a => t a -> a
    minimum1 = getMin . foldMap1 Min

traverse1_ :: (Foldable1 t, Applicative f) => (a -> f b) -> t a -> f ()
traverse1_ f = foldr ((*>) . f) pass

for1_ :: (Foldable1 t, Applicative f) => t a -> (a -> f b) -> f ()
for1_ = flip traverse1_

instance Foldable1 NonEmpty where
    foldMap1 f (a :| [])     = f a
    foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)
    toNonEmpty = id

instance Foldable1 Identity where
    foldMap1 f = f . runIdentity

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose
