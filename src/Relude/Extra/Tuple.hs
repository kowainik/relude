{- |
Copyright:  (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains utility functions for working with tuples.
-}

module Relude.Extra.Tuple
       ( dupe
       ) where


{- | Creates a tuple by pairing something with itself.

>>> dupe "foo"
("foo", "foo")
>>> dupe ()
((), ())
-}
dupe :: a -> (a, a)
dupe a = (a, a)
{-# INLINE dupe #-}