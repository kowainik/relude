{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Trustworthy           #-}

module Universum
       ( -- * Reexports from base and from modules in this repo
         module X
       , module Base

         -- * Useful classes
       , Buildable

         -- * Useful standard unclassifed functions
       , identity
       , map
       , uncons
       , unsnoc
       , pretty
       , prettyL
       , print
       , foreach
       , guarded
       , show

         -- * Convenient type aliases
       , LText
       , LByteString
       ) where

import           Applicative              as X
import           Bool                     as X
import           Containers               as X
import           Conv                     as X
import           Debug                    as X
import           Exceptions               as X
import           Functor                  as X
import           Lifted                   as X
import           List                     as X
import           Monad                    as X
import           Show                     as X
import           TypeOps                  as X

import           Base                     as Base hiding (error, print, putStr, putStrLn,
                                                   show, showFloat, showList, showSigned,
                                                   showSignedFloat, showsPrec, undefined)
import qualified Base                     as PBase

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
import           Data.Eq                  as X (Eq (..))
import           Data.Foldable            as X (Foldable, concat, concatMap, foldlM,
                                                foldrM, maximumBy, minimumBy)
import           Data.Functor.Identity    as X (Identity (..))
import           Data.Ord                 as X (Down (..), Ord (..), Ordering (..),
                                                comparing)
import           Data.Traversable         as X hiding (for)

#if ( __GLASGOW_HASKELL__ >= 800 )
import           Data.List.NonEmpty       as X (NonEmpty (..), nonEmpty)
import           Data.Monoid              as X
import           Data.Semigroup           as X (Option (..), Semigroup (sconcat, stimes),
                                                WrappedMonoid, cycle1, mtimesDefault,
                                                stimesIdempotent, stimesIdempotentMonoid,
                                                stimesMonoid)
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
import           Data.Hashable            as X (Hashable)
import           Data.HashMap.Strict      as X (HashMap)
import           Data.HashSet             as X (HashSet)
import           Data.IntMap.Strict       as X (IntMap)
import           Data.IntSet              as X (IntSet)
import           Data.List                as X (break, cycle, drop, dropWhile, filter,
                                                genericDrop, genericLength,
                                                genericReplicate, genericSplitAt,
                                                genericTake, group, inits, intercalate,
                                                intersperse, isPrefixOf, iterate,
                                                permutations, repeat, replicate, reverse,
                                                scanl, scanr, sort, sortBy, splitAt,
                                                subsequences, tails, take, takeWhile,
                                                transpose, unfoldr, zip, zipWith)
import           Data.Map.Strict          as X (Map)
import           Data.Sequence            as X (Seq)
import           Data.Set                 as X (Set)
import           Data.Tuple               as X (curry, fst, snd, swap, uncurry)

#if ( __GLASGOW_HASKELL__ >= 710 )
import           Data.Proxy               as X (Proxy (..))
import           Data.Typeable            as X (Typeable)
import           Data.Void                as X (Void, absurd, vacuous)
#endif

-- Base types
import           Data.Bits                as X hiding (unsafeShiftL, unsafeShiftR)
import           Data.Bool                as X hiding (bool)
import           Data.Char                as X (chr)
import           Data.Int                 as X (Int, Int16, Int32, Int64, Int8)
import           Data.Maybe               as X hiding (fromJust)
import           Data.Word                as X (Word, Word16, Word32, Word64, Word8,
                                                byteSwap16, byteSwap32, byteSwap64)

import           Data.Function            as X (const, fix, flip, on, ($), (.))

-- Generics and type level magic
import           GHC.Generics             as X (Generic)
#if ( __GLASGOW_HASKELL__ >= 710 )
import           GHC.TypeLits             as X (CmpNat, KnownNat, KnownSymbol, Nat,
                                                SomeNat (..), SomeSymbol (..), Symbol,
                                                natVal, someNatVal, someSymbolVal,
                                                symbolVal)
#endif

-- Buildable
import           Data.Text.Buildable      (Buildable (build))
import           Data.Text.Lazy.Builder   (toLazyText)

-- ByteString
import           Data.ByteString          as X (ByteString)
import qualified Data.ByteString.Lazy

-- Text
import           Data.Text                as X (Text, lines, unlines, unwords, words)
import qualified Data.Text.Lazy

import           Data.Text.Lazy           as X (fromStrict, toStrict)

import           Data.Text.Encoding       as X (decodeUtf8', decodeUtf8With)
import           Data.Text.Encoding.Error as X (OnDecodeError, OnError, UnicodeException,
                                                lenientDecode, strictDecode)

-- IO
import           System.IO                as X (FilePath, Handle, IOMode (..), stderr,
                                                stdin, stdout, withFile)

-- ST
import           Control.Monad.ST         as X (ST, fixST, runST)

-- Concurrency and Parallelism
import           Control.Exception        as X (Exception, SomeException (..))

import           Control.Concurrent       as X hiding (ThreadId, getNumCapabilities,
                                                isCurrentThreadBound, killThread,
                                                mkWeakThreadId, myThreadId,
                                                setNumCapabilities, threadCapability,
                                                throwTo)
import           Control.Concurrent.Async as X (Async (..), Concurrently (..), async,
                                                asyncBound, asyncOn, asyncThreadId,
                                                cancel, cancelWith, concurrently, link,
                                                link2, poll, race, race_, waitAny,
                                                waitAnyCancel, waitAnyCatch,
                                                waitAnyCatchCancel, waitBoth, waitCatch,
                                                waitEither, waitEitherCancel,
                                                waitEitherCatch, waitEitherCatchCancel,
                                                waitEither_, withAsync, withAsyncBound,
                                                withAsyncOn)
import           Control.Monad.STM        as X (STM, always, alwaysSucceeds, catchSTM,
                                                check, orElse, retry, throwSTM)

import           Foreign.Storable         as X (Storable)

-- Read instances hiding unsafe builtins (read)
import           Text.Read                as X (Read, readEither, readMaybe, reads)

-- Lenses
import           Lens.Micro               as X (Lens, Lens', Traversal, Traversal', over,
                                                set, (%~), (&), (.~), (<&>), (^.), (^..),
                                                (^?), _1, _2, _3, _4, _5)
import           Lens.Micro.Mtl           as X (preuse, preview, use, view)

-- Type synonyms for lazy types
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString

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

print :: (X.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = X.bool empty (pure x) (p x)

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
