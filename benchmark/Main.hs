{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.DeepSeq    (NFData)
import           Criterion.Main     (Benchmark, bench, bgroup, defaultMain, nf)
import           Data.Hashable      (Hashable)
import qualified Data.HashSet       as HSet
import           Data.List          (group, head, nub, sort, zip5)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as T

import           List               (hashNub, ordNub)
import           VarArg             ((...))

main :: IO ()
main = defaultMain
  [ bgroupList listOfSmall    "small"
  , bgroupList listOfBig      "big"
  , bgroupList (nStrings 'z') "small str"
  , bgroupList (nStrings 'c') "big str"
  , bgroupSuperComposition
  ]

bgroupList :: forall a .
              (Ord a, Hashable a, NFData a)
           => (Int -> [a])
           -> String
           -> Benchmark
bgroupList f name = bgroup name $
  map ($ f)
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
      [ bench "ordNub"   $ nf ordNub     (listN :: [a])
      , bench "hashNub"  $ nf hashNub    (listN :: [a])
      , bench "set"      $ nf setNub     (listN :: [a])
      , bench "hashSet"  $ nf hashSetNub (listN :: [a])
      , bench "sort"     $ nf groupSort  (listN :: [a])
      , bench "safeSort" $ nf safeSort   (listN :: [a])
      ]

  setNub :: [a] -> [a]
  setNub = Set.toList . Set.fromList

  hashSetNub :: [a] -> [a]
  hashSetNub = HSet.toList . HSet.fromList

  groupSort :: [a] -> [a]
  groupSort = map head . group . sort

  safeSort :: [a] -> [a]
  safeSort = map NonEmpty.head . NonEmpty.group . sort

listOfSmall :: Int -> [Int]
listOfSmall n = let part = n `div` 100 in concat $ replicate part [1..100]

listOfBig :: Int -> [Int]
listOfBig n = let part = n `div` 2 in [1..part] ++ [1..part]

allStrings :: Char -> [String]
allStrings ch =  [ c : s | s <- "" : allStrings ch, c <- ['a'..ch] ]

nStrings :: Char -> Int -> [Text]
nStrings ch n = take n $ map T.pack $ allStrings ch

bgroupSuperComposition :: Benchmark
bgroupSuperComposition = bgroup "(...)"
  [ bgroup "show+" [ bench "super" $ nf (show ... (+) 1) (2 :: Int)
                   , bench "norm"  $ nf (show . ((+) 1)) (2 :: Int)
                   ]
  , bgroup "show"  [ bench "super" $ nf (show ...) (5 :: Int)
                   , bench "norm"  $ nf show       (5 :: Int)
                   ]
  , bgroup "zip5"  [ bench "super" $ nf ((null ... zip5) [()] [()] [()] []) [()]
                   , bench "norm"  $ nf (null . (zip5 [()] [()] [()] []) )  [()]
                   ]
  , bgroup "10x"   [ bench "super" $ nf super10 [()]
                   , bench "norm"  $ nf norm10  [()]
                   ]
  ]
 where
  super10 = null
        ... (: []) ... head ... pure ... head
        ... (: [(), (), (), ()]) ... head ... (: []) ... head
        ... (: [()]) ... head ... (: [(), ()]) ... head

  norm10 = null
         . (: []) . head . pure . head
         . (: [(), (), (), ()]) . head . (: []) . head
         . (: [()]) . head . (: [(), ()]) . head
