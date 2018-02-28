module Main where

import Universum hiding (show)

import Data.List (nub, zip5)
import Gauge (Benchmark, bench, bgroup, nf, whnf)
import Gauge.Main (defaultMain)
import Prelude (show)

import qualified Data.Foldable as Foldable (elem)
import qualified Data.HashSet as HashSet (fromList, insert)
import qualified Data.List.NonEmpty as NonEmpty (group, head)
import qualified Data.Set as Set (fromList)
import qualified Universum.Container as Container (elem)
import qualified Universum.Unsafe as Unsafe

main :: IO ()
main = defaultMain
  [ bgroupList listOfSmall    "small"
  , bgroupList listOfBig      "big"
  , bgroupList (nStrings 'z') "small str"
  , bgroupList (nStrings 'c') "big str"
  , bgroupSuperComposition
  , bgroupConcatMap
  , bgroupMember
  , bgroupFold
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

bgroupSuperComposition :: Benchmark
bgroupSuperComposition = bgroup "(...)"
  [ bgroup "show+" [ bench "super" $ nf (show ... (+ 1)) (2 :: Int)
                   , bench "norm"  $ nf (show  .  (+ 1)) (2 :: Int)
                   ]
  , bgroup "show"  [ bench "super" $ nf (show ...) (5 :: Int)
                   , bench "norm"  $ nf show       (5 :: Int)
                   ]
  , bgroup "zip5"  [ bench "super" $ nf ((null ... zip5) [()] [()] [()] []) [()]
                   , bench "norm"  $ nf (null   .  zip5  [()] [()] [()] []) [()]
                   ]
  , bgroup "10x"   [ bench "super" $ nf super10 [()]
                   , bench "norm"  $ nf norm10  [()]
                   ]
  , bgroup "5arg"  [ bench "super" $ nf (\x -> super5arg x x x x x) [()]
                   , bench "norm"  $ nf (\x -> norm5args x x x x x) [()]
                   , bench "unty"  $ nf (\x -> unty5args x x x x x) [()]
                   , bench "line"  $ nf (\x -> line5args x x x x x) [()]
                   ]
  ]
 where
  super10 = null
        ... (: []) ... Unsafe.head ... pure ... Unsafe.head
        ... (: [(), (), (), ()]) ... Unsafe.head ... (: []) ... Unsafe.head
        ... (: [()]) ... Unsafe.head ... (: [(), ()]) ... Unsafe.head

  norm10 = null
         . (: []) . Unsafe.head . pure . Unsafe.head
         . (: [(), (), (), ()]) . Unsafe.head . (: []) . Unsafe.head
         . (: [()]) . Unsafe.head . (: [(), ()]) . Unsafe.head

  super5arg :: [()] -> [()] -> [()] -> [()] -> [()] -> Bool
  super5arg = super10 ... map fst5 ... zip5

  unty5args = super10 ... map fst5 ... zip5

  line5args = super10 ... map fst5 ... zip5
  {-# INLINE line5args #-}

  norm5args :: [()] -> [()] -> [()] -> [()] -> [()] -> Bool
  norm5args a b c d = norm10 . map fst5 . zip5 a b c d

  fst5 :: (a,b,c,d,e) -> a
  fst5 (a, _, _, _, _) = a

bgroupConcatMap :: Benchmark
bgroupConcatMap = bgroup "concat"
  [ concatGroup 10
  , concatGroup 100
  , concatGroup 1000
  ]
 where
  concatGroup :: Int -> Benchmark
  concatGroup n = bgroup (show n)
    [ bench "simple"   $ nf concatSimple n
    , bench "identity" $ nf concatIdentity n
    ]

  concatSimple :: Int -> [()]
  concatSimple n = concatMap pure $ replicate n ()

  concatIdentity :: Int -> Identity [()]
  concatIdentity n = concatMapM (Identity . pure) $ replicate n ()

-- | Checks that 'member' is implemented efficiently for 'Set' and 'HashSet'.
bgroupMember :: Benchmark
bgroupMember = do
    let testList    = [1..100000] :: [Int]
    let sample      = 50000
    let listSet     = Set.fromList     testList
    let listHashSet = HashSet.fromList testList
    bgroup "member" [ bench "Set/foldable"     $ whnf (Foldable.elem  sample) listSet
                    , bench "Set/elem"         $ whnf (Container.elem sample) listSet
                    , bench "HashSet/Foldable" $ whnf (Foldable.elem  sample) listHashSet
                    , bench "HashSet/elem"     $ whnf (Container.elem sample) listHashSet
                    ]

-- | Checks that 'foldl'' is implemented efficiently for 'Universum.List'
bgroupFold :: Benchmark
bgroupFold = do
    let testList   = [1..100000] :: [Int]
    let flipFoldl' = flipfoldl' HashSet.insert mempty
    let ghcFoldl'  = foldl' (\hashSet element -> HashSet.insert element hashSet) mempty
    bgroup "foldl'" [ bench "flipped" $ nf flipFoldl' testList
                    , bench "base"    $ nf ghcFoldl'  testList
                    ]
