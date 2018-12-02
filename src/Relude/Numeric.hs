{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Copyright: (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Relude.Numeric
       ( module Data.Bits
       , module Data.Int
       , module Data.Word
       , module Numeric
       , module GHC.Float
       , module GHC.Num
       , module GHC.Real
       , integerToBounded
       , integerToNatural
       )
       where

import Numeric

import Data.Bits (toIntegralSized, xor)
import Data.Function (($))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import GHC.Float (Double (..), Float (..), Floating (acos, acosh, asin, asinh, atan, atanh, cos, cosh, exp, logBase, pi, sin, sinh, sqrt, tan, tanh, (**)))
import GHC.Num (Integer, Num (..), subtract)
import GHC.Real (Fractional (..), Integral (..), Ratio, Rational, Real (..), RealFrac (..),
                 denominator, even, fromIntegral, gcd, lcm, numerator, odd, realToFrac, (^), (^^))
import Numeric.Natural (Natural)

import Relude.Base (Bounded (..), (<), (<=), (>))
import Relude.Bool (otherwise)
import Relude.Monad (Maybe (..))


-- $setup
-- import Relude.Monad (Maybe (..))

{- | Transforms an integer number to a bounded integral.
It returns `Nothing` for integers outside the bound of the return type.

>>> integerToBounded @Int 42
Just 42

>>> integerToBounded @Int8 1024
Nothing

-}
integerToBounded :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
integerToBounded n
  | n < toInteger (minBound :: a) = Nothing
  | n > toInteger (maxBound :: a) = Nothing
  | otherwise                    = Just (fromIntegral n)

{- | Tranforms an integer number to a natural.
Only non-negative integers are considered natural, everything else will return `Nothing`.

>>> integerToNatural (-1)
Nothing

>>> integerToNatural 0
Nothing

>>> integerToNatural 10
Just 10

-}
integerToNatural :: Integer -> Maybe Natural
integerToNatural n
  | n <= 0    = Nothing
  | otherwise = Just $ fromIntegral n
