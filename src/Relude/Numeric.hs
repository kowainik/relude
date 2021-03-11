{-# LANGUAGE Trustworthy #-}

{- |
Copyright:  (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Provides numerical data types and functions.

@since 0.5.0
-}

module Relude.Numeric
    ( -- * Reexports
      module Data.Bits
    , module Data.Int
    , module Data.Word
    , module GHC.Base
    , module GHC.Float
    , module GHC.Num
    , module GHC.Real
    , module Numeric.Natural
      -- * Combinators
    , integerToBounded
    , integerToNatural
    ) where

import Data.Bits (toIntegralSized, xor)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import GHC.Base (maxInt, minInt)
import GHC.Float (Double (..), Float (..), Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, logBase, pi, sin, sinh, sqrt, tan, tanh, (**)),
                  RealFloat (atan2, decodeFloat, encodeFloat, floatDigits, floatRadix, floatRange, isDenormalized, isIEEE, isInfinite, isNaN, isNegativeZero))
import GHC.Num (Integer, Num (..), subtract)
import GHC.Real (Fractional (..), Integral (..), Ratio, Rational, Real (..), RealFrac (..),
                 denominator, even, fromIntegral, gcd, lcm, numerator, odd, realToFrac, (^), (^^))
import Numeric.Natural (Natural)

import Relude.Base ((<), (>))
import Relude.Bool (otherwise)
import Relude.Enum (Bounded (..))
import Relude.Function (($))
import Relude.Monad (Maybe (..))


-- $setup
-- import Relude.Monad (Maybe (..))

{- | Transforms an integer number to a bounded integral.
It returns `Nothing` for integers outside the bound of the return type.

>>> integerToBounded @Int 42
Just 42

>>> integerToBounded @Int8 1024
Nothing

>>> integerToBounded @Int (toInteger (minBound :: Int))
Just (-9223372036854775808)
>>> integerToBounded @Int $ (toInteger (minBound :: Int)) - 1
Nothing

>>> integerToBounded @Int (toInteger (maxBound :: Int))
Just 9223372036854775807
>>> integerToBounded @Int $ (toInteger (maxBound :: Int)) + 1
Nothing

If you want to convert 'Int' or 'Word' to a bounded type, take a look at
'toIntegralSized' function instead.

@since 0.5.0
-}
integerToBounded :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
integerToBounded n
    | n < toInteger (minBound @a) = Nothing
    | n > toInteger (maxBound @a) = Nothing
    | otherwise                   = Just (fromIntegral n)
{-# INLINE integerToBounded #-}

{- | Transforms an integer number to a natural.
Only non-negative integers are considered natural, everything else will return `Nothing`.

>>> integerToNatural (-1)
Nothing

>>> integerToNatural 0
Just 0

>>> integerToNatural 10
Just 10

@since 0.5.0
-}
integerToNatural :: Integer -> Maybe Natural
integerToNatural n
    | n < 0     = Nothing
    | otherwise = Just $ fromIntegral n
{-# INLINE integerToNatural #-}
