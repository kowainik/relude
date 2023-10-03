{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module                  : Relude.Extra.Foldable1
Copyright               : (c) 2011-2015 Edward Kmett
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

'Foldable1' is a typeclass like 'Data.Foldable.Foldable' but for non-empty
structures. For example, 'Relude.NonEmpty', 'Relude.Identity'.

'Foldable1' has all type-safe and total methods like `head1`, `maximum1` in
contradiction with 'Data.Foldable.Foldable'.

@since 0.3.0
-}

module Relude.Extra.Foldable1
    ( Foldable1 (..)
    , foldl1'
    , average1
    ) where

import Relude hiding (Product (..), Sum (..))
import Relude.Extra.Newtype ((#.))

import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

import qualified Data.Semigroup as SG


-- $setup
-- >>> import Relude

{- | The class of foldable data structures that cannot be empty.

@since 0.3.0
-}
class Foldable f => Foldable1 f where
    {-# MINIMAL foldMap1 #-}

    {- | Map each element of the non-empty structure to a semigroup, and combine the results.

    >>> foldMap1 SG.Sum (1 :| [2, 3, 4])
    Sum {getSum = 10}
    >>> foldMap1 show (123 :| [456, 789, 0])
    "1234567890"
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

    {- | Combines the elements of a non-empty structure using a binary function @f@.

    >>> foldr1 (+) 0 (1 :| [2, 3])
    6
    >>> foldr1 (+) 1 $ Identity 3
    4

    @since 1.0.0.0
    -}
    foldr1 :: (a -> b -> b) -> b -> f a -> b
    foldr1 f accum as = appEndo (foldMap1 (Endo #. f) as) accum
    {-# INLINE foldr1 #-}

    {- | Convert a non-empty data structure to a NonEmpty list.

    >>> toNonEmpty (Identity 2)
    2 :| []
    -}
    toNonEmpty :: f a -> NonEmpty a
    toNonEmpty = foldMap1 (:| [])

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

    {- | The smallest element of a non-empty data structure.

    >>> minimum1 (32 :| [64, 8, 128, 16])
    8
    -}
    minimum1 :: Ord a => f a -> a
    minimum1 = SG.getMin #. foldMap1 SG.Min

    {- | The largest element of a non-empty data structure
         with respect to the given comparison function.

    >>> maximumOn1 abs (0 :| [2, 1, -3, -2])
    -3

    @since 1.0.0.0
    -}
    maximumOn1 :: Ord b => (a -> b) -> f a -> a
    maximumOn1 f = maximumOn1 f . toNonEmpty
    {-# INLINE maximumOn1 #-}

    {- | The smallest element of a non-empty data structure
         with respect to the given comparison function.

    >>> minimumOn1 abs (0 :| [2, 1, -3, -2])
    0

    @since 1.0.0.0
    -}
    minimumOn1 :: Ord b => (a -> b) -> f a -> a
    minimumOn1 f = minimumOn1 f . toNonEmpty
    {-# INLINE minimumOn1 #-}

{- |

@since 0.3.0
-}
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

    maximumOn1 :: forall a b. Ord b => (a -> b) -> NonEmpty a -> a
    maximumOn1 func = foldl1' $ cmpOn
      where
        cmpOn :: a -> a -> a
        cmpOn a b = case func a `compare` func b of
                        GT -> a
                        _  -> b
    {-# INLINE maximumOn1 #-}

    minimumOn1 :: forall a b. Ord b => (a -> b) -> NonEmpty a -> a
    minimumOn1 func = foldl1' $ cmpOn
      where
        cmpOn :: a -> a -> a
        cmpOn a b = case func a `compare` func b of
                        LT -> a
                        _  -> b
    {-# INLINE minimumOn1 #-}

{- |

@since 0.3.0
-}
instance Foldable1 Identity where
    foldMap1 :: Semigroup m => (a -> m) -> Identity a -> m
    foldMap1 = coerce
    {-# INLINE foldMap1 #-}

    fold1 :: Semigroup m => Identity m -> m
    fold1 = coerce
    {-# INLINE fold1 #-}

    toNonEmpty :: Identity a -> NonEmpty a
    toNonEmpty = (:| []) . coerce
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

    maximumOn1 :: Ord b => (a -> b) -> Identity a -> a
    maximumOn1 = const coerce
    {-# INLINE maximumOn1 #-}

    minimumOn1 :: Ord b => (a -> b) -> Identity a -> a
    minimumOn1 = const coerce
    {-# INLINE minimumOn1 #-}

{- |

@since 0.3.0
-}
instance Foldable1 ((,) c) where
    foldMap1 :: Semigroup m => (a -> m) -> (c, a) -> m
    foldMap1 f = f . snd
    {-# INLINE foldMap1 #-}

    fold1 :: Semigroup m => (c, m) -> m
    fold1 = snd
    {-# INLINE fold1 #-}

    toNonEmpty :: (c, a) -> NonEmpty a
    toNonEmpty (_, y) = y :| []
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

    maximumOn1, minimumOn1 :: Ord b => (a -> b) -> (c, a) -> a
    maximumOn1 = const snd
    minimumOn1 = const snd
    {-# INLINE maximumOn1 #-}
    {-# INLINE minimumOn1 #-}

{- |

@since 0.3.0
-}
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

{- |

@since 0.3.0
-}
instance (Foldable1 f, Foldable1 g) => Foldable1 (Product f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Product f g a -> m
    foldMap1 f (Pair a b) = foldMap1 f a <> foldMap1 f b
    {-# INLINE foldMap1 #-}

{- |

@since 0.3.0
-}
instance (Foldable1 f, Foldable1 g) => Foldable1 (Sum f g) where
    foldMap1 :: Semigroup m => (a -> m) -> Sum f g a -> m
    foldMap1 f (InL x) = foldMap1 f x
    foldMap1 f (InR y) = foldMap1 f y
    {-# INLINE foldMap1 #-}


----------------------------------------------------------------------------
-- List custom error
----------------------------------------------------------------------------

-- | For tracking usage of ordinary list with 'Foldable1' functions.
type family IsListError :: Constraint
  where
    IsListError = TypeError
        ( 'Text "The methods of the 'Foldable1' type class work with non-empty containers."
        ':$$: 'Text "However, one of the 'Foldable1' functions is applied to the List."
        ':$$: 'Text ""
        ':$$: 'Text "Possible fixes:"
        ':$$: 'Text "  * Replace []"
        ':$$: 'Text "    with one of the: 'NonEmpty', 'Identity', '(c,)', 'Compose f g', 'Product f g', 'Sum f g'"
        ':$$: 'Text "  * Or use 'Foldable' class for your own risk."
        )


{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

'Foldable1' is not supposed to be used with the lists.

In case it is used by mistake, the user will see the following:

>>> head1 [1, 2, 3]
...
... The methods of the 'Foldable1' type class work with non-empty containers.
      However, one of the 'Foldable1' functions is applied to the List.
...
      Possible fixes:
        * Replace []
          with one of the: 'NonEmpty', 'Identity', '(c,)', 'Compose f g', 'Product f g', 'Sum f g'
        * Or use 'Foldable' class for your own risk.
...

@since 0.6.0.0
-}
instance IsListError => Foldable1 [] where
    foldMap1 :: Semigroup m => (a -> m) -> [a] -> m
    foldMap1 _ _ = error "Unreachable list instance of Foldable1"

    foldr1 :: (Foldable1 f) => (a -> b -> b) -> b -> f a -> b
    foldr1 _ _ = error "Unreachable list instance of Foldable1"

    fold1 :: Semigroup m => [m] -> m
    fold1 _ = error "Unreachable list instance of Foldable1"

    toNonEmpty :: [a] -> NonEmpty a
    toNonEmpty _ = error "Unreachable list instance of Foldable1"

    head1 :: [a] -> a
    head1 _ = error "Unreachable list instance of Foldable1"

    last1 :: [a] -> a
    last1 _ = error "Unreachable list instance of Foldable1"

    maximum1 :: Ord a => [a] -> a
    maximum1 _ = error "Unreachable list instance of Foldable1"

    minimum1 :: Ord a => [a] -> a
    minimum1 _ = error "Unreachable list instance of Foldable1"

    maximumOn1 :: (Ord b, Foldable1 f) => (a -> b) -> f a -> a
    maximumOn1 _ _ = error "Unreachable list instance of Foldable1"

    minimumOn1 :: (Ord b, Foldable1 f) => (a -> b) -> f a -> a
    minimumOn1 _ _ = error "Unreachable list instance of Foldable1"

{- | Strictly folds non-empty structure with given function @f@:

@
foldl1' f [x0, x1, x2 ...] = f (f x0 x1) x2 ...
@

>>> foldl1' (++) ([1,2] :| [[3,4], [5,6]])
[1,2,3,4,5,6]

@since 0.3.0
-}
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' _ (x :| [])     = x
foldl1' f (x :| (y:ys)) = foldl' f (f x y) ys
{-# INLINE foldl1' #-}

{- |  Given a 'Foldable1' of 'Fractional' elements, computes the average if
possible and returns the resulting element.


>>> average1 (42 :| [])
42.0
>>> average1 (1 :| [2,3,4])
2.5

@since 1.0.0.0
-}
average1 :: forall a f . (Foldable1 f, Fractional a) => f a -> a
average1 = uncurry (/) . foldl' (\(!total, !count) x -> (total + x, count + 1)) (0, 0)
{-# INLINE average1 #-}

{-
Copyright 2011-2015 Edward Kmett

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}
