{-# LANGUAGE NoImplicitPrelude #-}

module Unsafe where

import qualified Prelude
import qualified Data.Maybe
import qualified Data.List

unsafeHead :: [a] -> a
unsafeHead = Prelude.head

unsafeTail :: [a] -> [a]
unsafeTail = Prelude.tail

unsafeInit :: [a] -> [a]
unsafeInit = Prelude.init

unsafeLast :: [a] -> a
unsafeLast = Prelude.last

fromJust :: Prelude.Maybe a -> a
fromJust = Data.Maybe.fromJust

unsafeIndex :: [a] -> Prelude.Int -> a
unsafeIndex = (Data.List.!!)

(!!) :: [a] -> Prelude.Int -> a
(!!) = (Data.List.!!)
