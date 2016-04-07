{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Protolude (
  module X,
  identity,
  bool,
  (&),
  uncons,
  applyN,
  print,

  LText,
  LByteString,
) where

import qualified Prelude as P

import qualified List as X
import qualified Show as X
import qualified Bool as X
import qualified Debug as X
import qualified Monad as X
import qualified Applicative as X

-- Maybe'ized version of partial functions
import Safe as X (
    headMay
  , initMay
  , tailMay
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
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  )
import Data.Semiring as X
import Data.Functor.Identity as X
import Data.Functor as X (
    Functor(..)
  , ($>)
  , (<$>)
  , void
  )

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
  )
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)

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
import Data.Char as X (Char)
import Data.Maybe as X hiding (fromJust)
import Data.Either as X
import Data.Complex as X

import Data.Function as X (
    id
  , const
  , (.)
  , flip
  , fix
  , on
  )

-- Base GHC types
import GHC.IO as X (IO)
import GHC.Num as X
import GHC.Real as X
import GHC.Float as X
import GHC.Show as X
import GHC.Exts as X (
    Constraint
  , Ptr
  , FunPtr
  , the
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
  , NoSelector
  , Rec0
  , Par0
  , Constructor(..)
  , Selector(..)
  , Arity(..)
  , Fixity(..)
  )

-- ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString as X (ByteString)

-- Text
import Data.Text as X (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.IO
import Text.Printf as X (printf)

import Data.Text.Lazy (
    toStrict
  , fromStrict
  )

import Data.String.Conv as X (
    strConv
  , toS
  , toSL
  , Leniency(..)
  )

-- Printf
import Text.Printf as Exports (
    PrintfArg
  , printf
  , hPrintf
  )

-- IO
import System.Exit as X
import System.Environment as X (getArgs)
import System.IO as X (
    Handle
  , hClose
  )

-- ST
import Control.Monad.ST as ST

-- Concurrency and Parallelism
import Control.Exception as X
import Control.Concurrent as X
import Control.Concurrent.Async as X


import Foreign.Storable as Exports (Storable)

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

bool :: a -> a -> Bool -> a
bool f t b = if b then t else f

identity :: a -> a
identity x = x

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) id (X.replicate n f)

print :: (X.MonadIO m, P.Show a) => a -> m ()
print = liftIO . P.print
