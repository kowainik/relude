{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Protolude (
  module X,
  module Base,
  identity,
  map,
  (&),
  uncons,
  applyN,
  print,
  show,

  LText,
  LByteString,
) where

import List as X
import Show as X
import Bool as X
import Debug as X
import Monad as X
import Functor as X
import Either as X
import Applicative as X

import Base as Base hiding (
    putStr
  , putStrLn
  , print
  , show
  )
import qualified Base as PBase

-- Used for 'show'
import Data.String (String)

-- Maybe'ized version of partial functions
import Safe as X (
    headMay
  , headDef
  , initMay
  , initDef
  , initSafe
  , tailMay
  , tailDef
  , tailSafe
  , lastDef
  , lastMay
  , lookupJust
  , findJust
  , foldr1May
  , foldl1May
  , atMay
  , atDef
  )

-- Applicatives
import Control.Applicative as X (
    Applicative(..)
  , Alternative(..)
  , Const(..)
  , ZipList(..)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional
  )

-- Base typeclasses
import Data.Eq as X
import Data.Ord as X
import Data.Monoid as X
import Data.Traversable as X
import Data.Foldable as X hiding (
    foldr1
  , foldl1
  )
import Data.Semiring as X
import Data.Functor.Identity as X

#if (__GLASGOW_HASKELL__ >= 710)
import Data.Bifunctor as X (Bifunctor(..))
#else
import Bifunctor as X (Bifunctor(..))
#endif

-- Deepseq
import Control.DeepSeq as X (
    NFData(..)
  , ($!!)
  , deepseq
  , force
  )

-- Data structures
import Data.Tuple as X
import Data.List as X (
    splitAt
  , break
  , intercalate
  , isPrefixOf
  , drop
  , filter
  , reverse
  , replicate
  , take
  , sortBy
  , sort
  , intersperse
  , transpose
  , subsequences
  , permutations
  , scanl
  , scanr
  , iterate
  , repeat
  , cycle
  , unfoldr
  , takeWhile
  , dropWhile
  , group
  , inits
  , tails
  , zipWith
  , zip
  )
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)

#if ( __GLASGOW_HASKELL__ >= 710 )
import Data.Proxy as X (
    Proxy(..)
  )

import Data.Void as X (
    Void
  , absurd
  , vacuous
  )
#endif

-- Monad transformers
import Control.Monad.State as X (
    MonadState
  , State
  , StateT
  , put
  , get
  , gets
  , modify
  , withState

  , runState
  , execState
  , evalState

  , runStateT
  , execStateT
  , evalStateT
  )

import Control.Monad.Reader as X (
    MonadReader
  , Reader
  , ReaderT
  , ask
  , asks
  , local
  , runReader
  , runReaderT
  )

import Control.Monad.Except as X (
    MonadError
  , Except
  , ExceptT
  , throwError
  , catchError
  , runExcept
  , runExceptT
  )

import Control.Monad.Trans as X (
    MonadIO
  , lift
  , liftIO
  )

-- Base types
import Data.Int as X
import Data.Bits as X
import Data.Word as X
import Data.Bool as X hiding (bool)
import Data.Char as X (chr)
import Data.Maybe as X hiding (fromJust)
import Data.Either as X
import Data.Complex as X

import Data.Function as X (
    const
  , (.)
  , ($)
  , flip
  , fix
  , on
  )

-- Genericss
import GHC.Generics (
    Generic(..)
  , Rep
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)
  , (:*:)
  , Rec0
  , Constructor(..)
  , Selector(..)
  , Fixity(..)
  )

-- ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString as X (ByteString)

-- Text
import Data.Text as X (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.IO

import Data.Text.Lazy (
    toStrict
  , fromStrict
  )

import Data.String.Conv as X (
    strConv
  , toS
  , toSL
  , Leniency(..)
  , StringConv
  )

import Data.String as X (IsString)

-- Printf
import Text.Printf as X (
    PrintfArg
  , printf
  , hPrintf
  )

-- IO
import System.Exit as X
--import System.Info as X
import System.Environment as X (getArgs)
import System.IO as X (
    Handle
  , hClose
  )

-- ST
import Control.Monad.ST as X

-- Concurrency and Parallelism
import Control.Exception as X
import Control.Monad.STM as X
import Control.Concurrent as X
import Control.Concurrent.Async as X

import Foreign.Storable as X (Storable)

-- Read instances hiding unsafe builtins (read)
import Text.Read as X (
    Read
  , reads
  , readMaybe
  , readEither
  )

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

identity :: a -> a
identity x = x

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) identity (X.replicate n f)

print :: (X.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

show :: (Show a, StringConv String b) => a -> b
show x = toS (PBase.show x)
