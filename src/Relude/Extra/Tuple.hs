{-# LANGUAGE TupleSections #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains utility functions for working with tuples.
-}

module Relude.Extra.Tuple
       ( dup
       , mapToFst
       , mapToSnd
       , mapBoth
       , traverseToFst
       , traverseToSnd
       , traverseBoth
       ) where

import Relude


{- | Creates a tuple by pairing something with itself.

>>> dup "foo"
("foo","foo")
>>> dup ()
((),())

@since 0.6.0.0
-}
dup :: a -> (a, a)
dup a = (a, a)
{-# INLINE dup #-}

{- | Apply a function, with the result in the fst slot,
and the value in the other.

A dual to 'mapToSnd'

>>> mapToFst (+1) 10
(11,10)
-}
mapToFst :: (a -> b) -> a -> (b, a)
mapToFst f a = (f a, a)
{-# INLINE mapToFst #-}

{- | Apply a function, with the result in the second slot,
and the value in the other.

A dual to 'mapToFst'.

>>> mapToSnd (+1) 10
(10,11)
-}
mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)
{-# INLINE mapToSnd #-}

{- | Maps a function over both elements of a tuple.

>>> mapBoth ("Hello " <>) ("Alice", "Bob")
("Hello Alice","Hello Bob")
-}
mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a1, a2) = (f a1, f a2)
{-# DEPRECATED mapBoth "Use 'Relude.Extra.Bifunctor.bimapBoth' from \"Relude.Extra.Bifunctor\" instead" #-}
{-# INLINE mapBoth #-}

{- | Apply a function that returns a value inside of a functor,
with the output in the first slot, the input in the second,
and the entire tuple inside the functor.


A dual to 'traverseToSnd'

>>> traverseToFst (Just . (+1)) 10
Just (11,10)
>>> traverseToFst (const Nothing) 10
Nothing
-}
traverseToFst :: Functor t => (a -> t b) -> a -> t (b, a)
traverseToFst f a = (,a) <$> f a
{-# INLINE traverseToFst #-}

{- | Apply a function that returns a value inside of a functor,
with the output in the second slot, the input in the first,
and the entire tuple inside the functor.

A dual to 'traverseToFst'.

>>> traverseToSnd (Just . (+1)) 10
Just (10,11)
>>> traverseToSnd (const Nothing) 10
Nothing
-}
traverseToSnd :: Functor t => (a -> t b) -> a -> t (a, b)
traverseToSnd f a = (a,) <$> f a
{-# INLINE traverseToSnd #-}

{- | Maps a function that returns a value inside of
an applicative functor over both elements of a tuple,
with the entire tuple inside the applicative functor.

>>> traverseBoth (Just . ("Hello " <>)) ("Alice", "Bob")
Just ("Hello Alice","Hello Bob")
>>> traverseBoth (const Nothing) ("Alice", "Bob")
Nothing
-}
traverseBoth :: Applicative t => (a -> t b) -> (a, a) -> t (b, b)
traverseBoth f (a1, a2) = (,) <$> f a1 <*> f a2
{-# INLINE traverseBoth #-}
