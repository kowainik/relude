{-# LANGUAGE Unsafe #-}

-- | Unsafe functions to work with lists and 'Maybe'.
-- Sometimes unavoidable but better don't use them. This module
-- is not even included in default prelude exports.

module Unsafe
       ( unsafeHead
       , unsafeTail
       , unsafeInit
       , unsafeLast
       , unsafeFromJust
       , unsafeIndex
       ) where

import           Base       (Int)
import qualified Data.List  as List
import qualified Data.Maybe as Maybe

-- | Cautionary alias for 'List.head'.
unsafeHead :: [a] -> a
unsafeHead = List.head

-- | Cautionary alias for 'List.tail'.
unsafeTail :: [a] -> [a]
unsafeTail = List.tail

-- | Cautionary alias for 'List.init'.
unsafeInit :: [a] -> [a]
unsafeInit = List.init

-- | Cautionary alias for 'List.last'.
unsafeLast :: [a] -> a
unsafeLast = List.last

-- | Cautionary alias for 'List.!!'.
unsafeIndex :: [a] -> Int -> a
unsafeIndex = (List.!!)

-- | Cautionary alias for 'Maybe.fromJust'.
unsafeFromJust :: Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust
