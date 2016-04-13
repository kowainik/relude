{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Unsafe (
  unsafeHead,
  unsafeTail,
  unsafeInit,
  unsafeLast,
  fromJust,
  unsafeIndex,
) where

import Base (Int)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

unsafeHead :: [a] -> a
unsafeHead = List.head

unsafeTail :: [a] -> [a]
unsafeTail = List.tail

unsafeInit :: [a] -> [a]
unsafeInit = List.init

unsafeLast :: [a] -> a
unsafeLast = List.last

fromJust :: Maybe.Maybe a -> a
fromJust = Maybe.fromJust

unsafeIndex :: [a] -> Int -> a
unsafeIndex = (List.!!)
