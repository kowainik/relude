{-# LANGUAGE Unsafe #-}

{- | Unsafe functions to work with lists and 'Maybe'.
Sometimes unavoidable but better don't use them. This module
is intended to be imported qualified and it's not even included
in default prelude exports.

@
import qualified Universum.Unsafe as Unsafe

foo :: [a] -> a
foo = Unsafe.head
@

-}

module Universum.Unsafe
       ( head
       , tail
       , init
       , last
       , at
       , (!!)
       , fromJust
       ) where

import Data.List (head, init, last, tail, (!!))
import Data.Maybe (fromJust)

import Universum.Base (Int)
import Universum.Function (flip)

-- Non empty list definition for @liquidhaskell@.
{-@ type NonEmptyList a = {xs : [a] | len xs > 0} @-}

-- | Similar to '!!' but with flipped arguments.
{-@ at :: n : Nat -> {xs : NonEmptyList a | len xs > n} -> a @-}
at :: Int -> [a] -> a
at n xs = xs !! n
