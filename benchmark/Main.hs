{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

module Main where

import Relude hiding (show)

import Data.List (nub)
import Gauge (Benchmark, bench, bgroup, nf)
import Gauge.Main (defaultMain)
import Prelude (show)

import qualified Data.HashSet as HashSet (insert)
import qualified Data.List.NonEmpty as NonEmpty (group, head)
import qualified Relude.Unsafe as Unsafe

main :: IO ()
main = defaultMain
  [ bgroupList listOfSmall    "small"
  , bgroupList listOfBig      "big"
  , bgroupList (nStrings 'z') "small str"
  , bgroupList (nStrings 'c') "big str"
  , bgroupFold
  ]

bgroupList :: forall a .
              (Ord a, Hashable a, NFData a)
           => (Int -> [a])
           -> String
           -> Benchmark
bgroupList f name = bgroup name $ map ($ f)
  [ bgroupNubAll 100
  , bgroupNubAll 500
  , bgroupNubAll 1000
  , bgroupNubHugeList 5000
  , bgroupNubHugeList 500000
  , bgroupNubHugeList 1000000
  ]
 where
  bgroupNubAll :: Int -> (Int -> [a]) -> Benchmark
  bgroupNubAll = bgroupNub True

  bgroupNubHugeList :: Int -> (Int -> [a]) -> Benchmark
  bgroupNubHugeList = bgroupNub False

  bgroupNub :: Bool -> Int -> (Int -> [a]) -> Benchmark
  bgroupNub isNub n listOf =
    bgroup (show n) nubBenchs
   where
    listN :: [a]
    listN = listOf n

    nubBenchs :: [Benchmark]
    nubBenchs =
      (if isNub
      then (:) (bench "nub" $ nf nub listN)
      else id)
      [ bench "ordNub"    $ nf ordNub      (listN :: [a])
      , bench "hashNub"   $ nf hashNub     (listN :: [a])
      , bench "sortNub"   $ nf sortNub     (listN :: [a])
      , bench "hashSet"   $ nf unstableNub (listN :: [a])
      , bench "groupSort" $ nf groupSort   (listN :: [a])
      , bench "safeSort"  $ nf safeSort    (listN :: [a])
      ]

  groupSort :: [a] -> [a]
  groupSort = map Unsafe.head . group . sort

  safeSort :: [a] -> [a]
  safeSort = map NonEmpty.head . NonEmpty.group . sort

listOfSmall :: Int -> [Int]
listOfSmall n = let part = n `div` 100 in concat $ replicate part [1..100]

listOfBig :: Int -> [Int]
listOfBig n = let part = n `div` 2 in [1..part] ++ [1..part]

allStrings :: Char -> [String]
allStrings ch =  [ c : s | s <- "" : allStrings ch, c <- ['a'..ch] ]

nStrings :: Char -> Int -> [Text]
nStrings ch n = take n $ map toText $ allStrings ch

-- | Checks that 'foldl'' is implemented efficiently for 'Relude.List'
bgroupFold :: Benchmark
bgroupFold = do
    let testList   = [1..100000] :: [Int]
    let flipFoldl' = flipfoldl' HashSet.insert mempty
    let ghcFoldl'  = foldl' (\hashSet element -> HashSet.insert element hashSet) mempty
    bgroup "foldl'" [ bench "flipped" $ nf flipFoldl' testList
                    , bench "base"    $ nf ghcFoldl'  testList
                    ]
