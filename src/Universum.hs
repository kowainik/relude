{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}

-- | Main module that reexports all functionality allowed to use
-- without importing any other modules. Just add next lines to your
-- module to replace default ugly 'Prelude' with better one.
--
-- @
--     {-# LANGUAGE NoImplicitPrelude #-}
--
--     import Universum
-- @

module Universum
       ( -- * Reexports from base and from modules in this repo
         module X  -- Should I expand this to all modules to remove haddock warnings?
       , module Base

         -- * Useful standard unclassifed functions
       , evaluateNF
       , evaluateNF_
       , evaluateWHNF
       , evaluateWHNF_
       , identity
       , map
       , uncons
       , unsnoc
       ) where

import Universum.Applicative as X
import Universum.Bool as X
import Universum.Container as X
import Universum.Debug as X
import Universum.Exceptions as X
import Universum.Functor as X
import Universum.Lifted as X
import Universum.List as X
import Universum.Monad as X
import Universum.Nub as X
import Universum.Print as X
import Universum.String as X
import Universum.TypeOps as X
import Universum.VarArg as X

import Universum.Base as Base hiding (error, show, showFloat, showList, showSigned, showSignedFloat,
                               showsPrec, undefined)

-- Maybe'ized version of partial functions
import Safe as X (atDef, atMay, foldl1May, foldr1May, headDef, headMay, initDef, initMay, initSafe,
                  lastDef, lastMay, tailDef, tailMay, tailSafe)

-- Applicatives and Bifunctors and Arrows
import Control.Applicative as X (Alternative (..), Applicative (..), Const (..), ZipList (..),
                                 liftA, liftA2, liftA3, optional, (<**>))
import Control.Arrow as X ((&&&))
import Data.Bifunctor as X (Bifunctor (..))

-- Base typeclasses
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable, concat, concatMap, foldlM, foldrM, maximumBy, minimumBy)
import Data.Functor.Identity as X (Identity (..))
import Data.Ord as X (Down (..), Ord (..), Ordering (..), comparing)
import Data.Traversable as X (Traversable (..), fmapDefault, foldMapDefault, forM, mapAccumL,
                              mapAccumR)

#if ( __GLASGOW_HASKELL__ >= 800 )
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Monoid as X hiding ((<>))
import Data.Semigroup as X (Option (..), Semigroup (sconcat, stimes, (<>)), WrappedMonoid, cycle1,
                            mtimesDefault, stimesIdempotent, stimesIdempotentMonoid, stimesMonoid)
#else
import Data.Monoid as X hiding ((<>))
#endif

-- Deepseq
import Control.DeepSeq as X (NFData (..), deepseq, force, ($!!))

-- Data structures
import Data.Tuple as X (curry, fst, snd, swap, uncurry)

#if ( __GLASGOW_HASKELL__ >= 710 )
import Data.Proxy as X (Proxy (..))
import Data.Typeable as X (Typeable)
import Data.Void as X (Void, absurd, vacuous)
#endif

-- Base types
import Data.Bits as X (xor)
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.Char as X (chr)
import Data.Int as X (Int, Int16, Int32, Int64, Int8)
import Data.Word as X (Word, Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)

import Data.Function as X (const, fix, flip, on, ($), (.))

-- Generics
import GHC.Generics as X (Generic)

-- IO
import System.IO as X (FilePath, Handle, IOMode (..), stderr, stdin, stdout, withFile)

-- Lenses
import Lens.Micro as X (Lens, Lens', Traversal, Traversal', over, set, (%~), (&), (.~), (<&>), (^.),
                        (^..), (^?), _1, _2, _3, _4, _5)
import Lens.Micro.Mtl as X (preuse, preview, use, view)

-- For internal usage only
import qualified Control.Exception.Base (evaluate)

-- | Renamed version of 'Prelude.id'.
identity :: a -> a
identity x = x

-- | 'Prelude.map' generalized to 'Functor'.
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- | Destructuring list into its head and tail if possible. This function is total.
--
-- >>> uncons []
-- Nothing
-- >>> uncons [1..5]
-- Just (1,[2,3,4,5])
-- >>> uncons (5 : [1..5]) >>= \(f, l) -> pure $ f == length l
-- Just True
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

-- | Similar to 'uncons' but destructuring list into its last element and
-- everything before it.
unsnoc :: [x] -> Maybe ([x],x)
unsnoc = foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing      -> ([], x)
       Just (xs, e) -> (x:xs, e))

-- | Lifted alias for 'Control.Exception.Base.evaluate' with clearer name.
evaluateWHNF :: MonadIO m => a -> m a
evaluateWHNF = liftIO . Control.Exception.Base.evaluate

-- | Like 'evaluateWNHF' but discards value.
evaluateWHNF_ :: MonadIO m => a -> m ()
evaluateWHNF_ what = (`seq` ()) <$!> evaluateWHNF what

-- | Alias for @evaluateWHNF . force@ with clearer name.
evaluateNF :: (X.NFData a, MonadIO m) => a -> m a
evaluateNF = evaluateWHNF . force

-- | Alias for @evaluateWHNF . rnf@. Similar to 'evaluateNF'
-- but discards resulting value.
evaluateNF_ :: (X.NFData a, MonadIO m) => a -> m ()
evaluateNF_ = evaluateWHNF . rnf
