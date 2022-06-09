{-# LANGUAGE Safe          #-}
{-# LANGUAGE TupleSections #-}

{- |
Module                  : Relude.Extra.Tuple
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Contains utility functions for working with tuples.

@since 0.4.0
-}

module Relude.Extra.Tuple
    ( dup
    , toFst
    , toSnd
    , fmapToFst
    , fmapToSnd
    , mapToFst
    , mapToSnd
    , traverseToFst
    , traverseToSnd
    , traverseBoth
    ) where

import Relude.Function ((.))
import Relude.Functor (Functor (..), (<$>))
import Relude.Applicative (Applicative (..))


-- $setup
-- >>> import Relude

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

A dual to 'toSnd'.

>>> toFst length [3, 1, 0, 2]
(4,[3,1,0,2])
>>> toFst (+5) 10
(15,10)

@since 0.7.0.0
-}
toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
{-# INLINE toFst #-}

{- | Apply a function, with the result in the snd slot,
and the value in the other.

A dual to 'toFst'.

>>> toSnd length [3, 1, 0, 2]
([3,1,0,2],4)
>>> toSnd (+5) 10
(10,15)

@since 0.7.0.0
-}
toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

{- | Like 'fmap', but also keep the original value in the snd position.

A dual to 'fmapToSnd'.

>>> fmapToFst show [3, 10, 2]
[("3",3),("10",10),("2",2)]

@since 0.7.0.0
-}
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst
{-# INLINE fmapToFst #-}

{- | Like 'fmap', but also keep the original value in the fst position.

A dual to 'fmapToFst'.

>>> fmapToSnd show [3, 10, 2]
[(3,"3"),(10,"10"),(2,"2")]

@since 0.7.0.0
-}
fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd = fmap . toSnd
{-# INLINE fmapToSnd #-}

{- | Apply a function, with the result in the fst slot,
and the value in the other.

A dual to 'mapToSnd'

>>> mapToFst (+1) 10
(11,10)

@since 0.4.0
-}
mapToFst :: (a -> b) -> a -> (b, a)
mapToFst = toFst
{-# INLINE mapToFst #-}
{-# DEPRECATED mapToFst "Use 'toFst' from 'Relude.Extra.Tuple' instead" #-}

{- | Apply a function, with the result in the second slot,
and the value in the other.

A dual to 'mapToFst'.

>>> mapToSnd (+1) 10
(10,11)

@since 0.4.0
-}
mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd = toSnd
{-# INLINE mapToSnd #-}
{-# DEPRECATED mapToSnd "Use 'toSnd' from 'Relude.Extra.Tuple' instead" #-}

{- | Apply a function that returns a value inside of a functor,
with the output in the first slot, the input in the second,
and the entire tuple inside the functor.


A dual to 'traverseToSnd'

>>> traverseToFst (Just . (+1)) 10
Just (11,10)
>>> traverseToFst (const Nothing) 10
Nothing

@since 0.5.0
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

@since 0.5.0
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
