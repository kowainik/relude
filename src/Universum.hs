{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Trustworthy           #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Universum
       ( -- * Reexports from base and from modules in this repo
         module X
       , module Base

         -- * Useful classes
       , Buildable

         -- * Useful standard unclassifed functions
       , identity
       , map
       , (&)
       , uncons
       , unsnoc
       , applyN
       , pretty
       , prettyL
       , print
       , foreach
       , pass
       , guarded
       , guardedA
       , show

         -- * Convenient type aliases
       , LText
       , LByteString
       ) where

import           Applicative              as X
import           Bool                     as X
import           Conv                     as X
import           Debug                    as X
import           Either                   as X
import           Functor                  as X
import           List                     as X hiding (product, sum)
import           Monad                    as X
import           Panic                    as X
import           Show                     as X

import           Base                     as Base hiding (error, print, putStr, putStrLn,
                                                   show, showFloat, showList, showSigned,
                                                   showSignedFloat, showsPrec, undefined)
import qualified Base                     as PBase

-- Used for 'show', not exported.
import           Data.String              (String)
import           Data.String              as X (IsString (..))

-- Maybe'ized version of partial functions
import           Safe                     as X (atDef, atMay, foldl1May, foldr1May,
                                                headDef, headMay, initDef, initMay,
                                                initSafe, lastDef, lastMay, tailDef,
                                                tailMay, tailSafe)

-- Applicatives
import           Control.Applicative      as X (Alternative (..), Applicative (..),
                                                Const (..), ZipList (..), liftA, liftA2,
                                                liftA3, optional, (<**>))

-- Base typeclasses
import           Data.Eq                  as X
import           Data.Foldable            as X hiding (foldl1, foldr1)
import           Data.Functor.Identity    as X
import           Data.Ord                 as X
import           Data.Traversable         as X hiding (for)

#if ( __GLASGOW_HASKELL__ >= 800 )
import           Data.Monoid              as X hiding ((<>))
import           Data.Semigroup           as X (Semigroup (..))
#else
import           Data.Monoid              as X
#endif

#if (__GLASGOW_HASKELL__ >= 710)
import           Data.Bifunctor           as X (Bifunctor (..))
#else
import           Bifunctor                as X (Bifunctor (..))
#endif

-- Deepseq
import           Control.DeepSeq          as X (NFData (..), deepseq, force, ($!!))

-- Data structures
import           Data.List                as X (break, cycle, drop, dropWhile, filter,
                                                group, inits, intercalate, intersperse,
                                                isPrefixOf, iterate, permutations, repeat,
                                                replicate, reverse, scanl, scanr, sort,
                                                sortBy, splitAt, subsequences, tails,
                                                take, takeWhile, transpose, unfoldr, zip,
                                                zipWith)
import           Data.Tuple               as X

import           Data.HashMap.Strict      as X (HashMap)
import           Data.HashSet             as X (HashSet)
import           Data.IntMap.Strict       as X (IntMap)
import           Data.IntSet              as X (IntSet)
import           Data.Map.Strict          as X (Map)
import           Data.Sequence            as X (Seq)
import           Data.Set                 as X (Set)

#if ( __GLASGOW_HASKELL__ >= 710 )
import           Data.Proxy               as X (Proxy (..))
import           Data.Typeable            as X (Typeable)
import           Data.Void                as X (Void, absurd, vacuous)
#endif

-- Monad transformers
import           Control.Monad.Catch      as X (MonadCatch (catch), MonadMask (..),
                                                MonadThrow (throwM))
import           Control.Monad.State      as X (MonadState, State, StateT, evalState,
                                                evalStateT, execState, execStateT, gets,
                                                modify, runState, runStateT, state,
                                                withState)

import           Control.Monad.Reader     as X (MonadReader, Reader, ReaderT, ask, asks,
                                                local, reader, runReader, runReaderT)

import           Control.Monad.Trans      as X (MonadIO, lift, liftIO)

-- Base types
import           Data.Bits                as X hiding (unsafeShiftL, unsafeShiftR)
import           Data.Bool                as X hiding (bool)
import           Data.Char                as X (chr)
import           Data.Complex             as X
import           Data.Either              as X
import           Data.Int                 as X
import           Data.Maybe               as X hiding (fromJust)
import           Data.Word                as X

import           Data.Function            as X (const, fix, flip, on, ($), (.))

-- Generics
import           GHC.Generics             as X (Generic)

-- Buildable
import           Data.Text.Buildable      (Buildable (build))
import           Data.Text.Lazy.Builder   (toLazyText)

-- ByteString
import           Data.ByteString          as X (ByteString)
import qualified Data.ByteString.Lazy

-- Text
import           Data.Text                as X (Text)
import qualified Data.Text.Lazy

import           Data.Text.IO             as X (appendFile, getContents, getLine,
                                                interact, readFile, writeFile)

import           Data.Text.Lazy           as X (fromStrict, toStrict)

import           Data.Text.Encoding       as X (decodeUtf8', decodeUtf8With)

-- IO
import           System.Environment       as X (getArgs)
import           System.Exit              as X
import           System.IO                as X (FilePath, Handle, IOMode (..), openFile,
                                                stderr, stdin, stdout, withFile)

-- ST
import           Control.Monad.ST         as X

-- Concurrency and Parallelism
#if ( __GLASGOW_HASKELL__ >= 710 )
import           Control.Exception        as X hiding (assert, catch, displayException,
                                                ioError, mask, throw, throwIO, throwTo,
                                                uninterruptibleMask)
#else
import           Control.Exception        as X hiding (assert, catch, ioError, mask,
                                                throw, throwIO, throwTo,
                                                uninterruptibleMask)
#endif

import           Control.Concurrent       as X hiding (ThreadId, throwTo)
import           Control.Concurrent.Async as X hiding (wait)
import           Control.Monad.STM        as X

import           Foreign.Storable         as X (Storable)

-- Read instances hiding unsafe builtins (read)
import           Text.Read                as X (Read, readEither, readMaybe, reads)

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

unsnoc :: [x] -> Maybe ([x],x)
unsnoc = foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing      -> ([], x)
       Just (xs, e) -> (x:xs, e))

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) identity (X.replicate n f)

print :: (X.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap

pass :: Applicative f => f ()
pass = pure ()

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = X.bool empty (pure x) (p x)

guardedA :: (Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = X.bool empty (pure x) <$> p x

show :: (Show a, IsString b) => a -> b
show x = X.fromString (PBase.show x)
{-# SPECIALIZE show :: Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}

-- | Functions to show pretty output for buildable data types.
pretty :: Buildable a => a -> Text
pretty = X.toStrict . prettyL

prettyL :: Buildable a => a -> LText
prettyL = toLazyText . build
