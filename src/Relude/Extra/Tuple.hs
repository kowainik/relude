{- |
Copyright:  (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains utility functions for working with tuples.
-}

module Relude.Extra.Tuple
       ( dupe
       , mapToFst
       , mapToSnd
       , mapBoth
       ) where

-- $setup
-- >>> import Relude

{- | Creates a tuple by pairing something with itself.

>>> dupe "foo"
("foo","foo")
>>> dupe ()
((),())
-}
dupe :: a -> (a, a)
dupe a = (a, a)
{-# INLINE dupe #-}

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
{-# INLINE mapBoth #-}
