import           Criterion.Main
import           Data.Hashable
import qualified Data.HashSet       as HSet
import           Data.List          (group, head, nub, sort)
import qualified Data.List.NonEmpty as S
import qualified Data.Set           as Set

import           List               (hashNub, ordNub)

main :: IO ()
main = defaultMain
  [ bgroupList listOfSmall "small"
  , bgroupList listOfBig "big"
  ]

bgroupList :: (Int -> [Int]) -> String -> Benchmark
bgroupList f name = bgroup name $
  map ($ f)
  [ bgroupNubAll 100
  , bgroupNubAll 500
  , bgroupNubAll 1000
  , bgroupNubHugeList 5000
  , bgroupNubHugeList 500000
  , bgroupNubHugeList 1000000
  ]

bgroupNubAll :: Int -> (Int -> [Int]) -> Benchmark
bgroupNubAll = bgroupNub True

bgroupNubHugeList :: Int -> (Int -> [Int]) -> Benchmark
bgroupNubHugeList = bgroupNub False

bgroupNub :: Bool -> Int -> (Int -> [Int]) -> Benchmark
bgroupNub isNub n listOf =
  bgroup (show n) nubBenchs
 where
  listN :: [Int]
  listN = listOf n

  nubBenchs :: [Benchmark]
  nubBenchs =
    (if isNub
    then (:) (bench "nub" $ nf nub listN)
    else id)
    [ bench "ordNub"   $ nf ordNub     listN
    , bench "hashNub"  $ nf hashNub    listN
    , bench "set"      $ nf setNub     listN
    , bench "hashSet"  $ nf hashSetNub listN
    , bench "sort"     $ nf groupSort  listN
    , bench "safeSort" $ nf safeSort   listN
    ]

setNub :: Ord a => [a] -> [a]
setNub = Set.toList . Set.fromList

hashSetNub :: (Eq a, Hashable a) => [a] -> [a]
hashSetNub = HSet.toList . HSet.fromList

groupSort :: Ord a => [a] -> [a]
groupSort = map head . group . sort

safeSort :: Ord a => [a] -> [a]
safeSort = map S.head . S.group . sort

listOfSmall :: Int -> [Int]
listOfSmall n = let part = n `div` 100 in concat $ replicate part [1..100]

listOfBig :: Int -> [Int]
listOfBig n = let part = n `div` 2 in [1..part] ++ [1..part]
