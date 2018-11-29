{-# LANGUAGE RankNTypes #-}

{- |
Copyright: (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Relude.Numeric
       ( module Data.Bits
       , module Data.Int
       , module Numeric
       , module GHC.Float
       , module GHC.Num
       , module GHC.Real
       , integerToBounded
       , integerToNatural
       )
        where

import Numeric

import Data.Bits (toIntegralSized)
import Data.Function (($))
import Data.Int (Int, Int16, Int32, Int64, Int8)
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
--
-- >>> integerToBounded 42
-- Just 42
--
-- integerToBounded :: forall a. (Integral a, Bounded a) => Integer -> Maybe a
integerToBounded :: Int -> Maybe Int
integerToBounded n | n < minBound = Nothing
                   | n > maxBound = Nothing
                   | otherwise    = Just n

-- >>> integerToNatural (-1)
-- Nothing
--
-- >>> integerToNatural 0
-- Nothing
--
-- >>> integerToNatural 10
-- Just 10
integerToNatural :: Integer -> Maybe Natural
integerToNatural n | n <= 0    = Nothing
                   | otherwise = Just $ fromIntegral n
