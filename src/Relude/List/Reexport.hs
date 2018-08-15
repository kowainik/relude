{-# LANGUAGE Trustworthy #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module reexports functinons to work with list and 'NonEmpty'.
-}

module Relude.List.Reexport
       ( module Data.List
       , module Data.List.NonEmpty
       , module GHC.Exts
       ) where

import Data.List (break, cycle, drop, dropWhile, filter, genericDrop, genericLength,
                  genericReplicate, genericSplitAt, genericTake, group, inits, intercalate,
                  intersperse, isPrefixOf, iterate, map, permutations, repeat, replicate, reverse,
                  scanl, scanr, sort, sortBy, sortOn, splitAt, subsequences, tails, take, takeWhile,
                  transpose, unfoldr, unzip, unzip3, zip, zip3, zipWith, (++))
import Data.List.NonEmpty (NonEmpty (..), head, init, last, nonEmpty, tail)

import GHC.Exts (sortWith)
