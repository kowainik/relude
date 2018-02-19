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

-- | Similar to '!!' but with flipped arguments.
at :: Int -> [a] -> a
at = flip (!!)
