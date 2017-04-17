{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE Trustworthy             #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Reimagined approach for 'Foldable' type hierarchy. Forbids usages
-- of 'length' function and similar over 'Maybe' and other potentially unsafe
-- data types. It was proposed to use @-XTypeApplication@ for such cases.
-- But this approach is not robust enough because programmers are human and can
-- easily forget to do this. For discussion see this topic:
-- <https://www.reddit.com/r/haskell/comments/60r9hu/proposal_suggest_explicit_type_application_for/ Suggest explicit type application for Foldable length and friends>

module Containers
       (
         -- * Foldable-like classes and methods
         Element
       , Container(..)
       , NontrivialContainer(..)

       , sum
       , product

       , mapM_
       , forM_
       , traverse_
       , for_
       , sequenceA_
       , sequence_
       , asum

         -- * Others
       , One(..)
       ) where

import           Control.Applicative    (Alternative (..))
import           Control.Monad.Identity (Identity)
import           Data.Coerce            (Coercible, coerce)
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import           Data.Hashable          (Hashable)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (All(..), Any(..), First(..))
import           Data.Word              (Word8)
import           Prelude                hiding (all, any, Foldable (..), mapM_,
                                                sequence_)

#if __GLASGOW_HASKELL__ >= 800
import           GHC.Err                (errorWithoutStackTrace)
import           GHC.TypeLits           (ErrorMessage (..), TypeError)
#endif

#if ( __GLASGOW_HASKELL__ >= 800 )
import qualified Data.List.NonEmpty     as NE
#endif

import qualified Data.Sequence          as SEQ

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL

import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.IntMap            as IM
import qualified Data.IntSet            as IS
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS

import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as VU
import qualified Data.Vector.Primitive  as VP
import qualified Data.Vector.Storable   as VS

import           Applicative            (pass)

----------------------------------------------------------------------------
-- Containers (e.g. tuples aren't containers)
----------------------------------------------------------------------------

-- | Type of element for some container. Implemented as a type family because
-- some containers are monomorphic over element type (like 'T.Text', 'IS.IntSet', etc.)
-- so we can't implement nice interface using old higher-kinded types approach.
type family Element t

type instance Element (f a) = a
type instance Element T.Text = Char
type instance Element TL.Text = Char
type instance Element BS.ByteString = Word8
type instance Element BSL.ByteString = Word8
type instance Element IS.IntSet = Int

-- | Type class for container. Fully compatible with 'Foldable'.
-- Contains very small and safe subset of 'Foldable' functions.
class Container t where
  -- | Convert container to list of elements.
  --
  -- >>> toList (Just True)
  -- [True]
  -- >>> toList @Text "aba"
  -- "aba"
  -- >>> :t toList @Text "aba"
  -- toList @Text "aba" :: [Char]
  toList :: t -> [Element t]

  -- | Checks whether container is empty.
  --
  -- >>> null @Text ""
  -- True
  -- >>> null @Text "aba"
  -- False
  null :: t -> Bool

-- | This instance makes 'Container' compatible and overlappable by 'Foldable'.
instance {-# OVERLAPPABLE #-} Foldable f => Container (f a) where
  toList = F.toList
  {-# INLINE toList #-}
  null = F.null
  {-# INLINE null #-}

instance Container T.Text where
  toList = T.unpack
  {-# INLINE toList #-}
  null = T.null
  {-# INLINE null #-}

instance Container TL.Text where
  toList = TL.unpack
  {-# INLINE toList #-}
  null = TL.null
  {-# INLINE null #-}

instance Container BS.ByteString where
  toList = BS.unpack
  {-# INLINE toList #-}
  null = BS.null
  {-# INLINE null #-}

instance Container BSL.ByteString where
  toList = BSL.unpack
  {-# INLINE toList #-}
  null = BSL.null
  {-# INLINE null #-}

instance Container IS.IntSet where
  toList = IS.toList
  {-# INLINE toList #-}
  null = IS.null
  {-# INLINE null #-}

----------------------------------------------------------------------------
-- Additional operations that don't make much sense for e.g. Maybe
----------------------------------------------------------------------------

-- | A class for 'Container's that aren't trivial like 'Maybe' (e.g. can hold
-- more than one value)
class Container t => NontrivialContainer t where
  foldMap :: Monoid m => (Element t -> m) -> t -> m
  foldMap f = foldr (mappend . f) mempty
  {-# INLINE foldMap #-}

  fold :: Monoid (Element t) => t -> Element t
  fold = foldMap id

  foldr :: (Element t -> b -> b) -> b -> t -> b
  foldr' :: (Element t -> b -> b) -> b -> t -> b
  foldr' f z0 xs = foldl f' id xs z0
    where f' k x z = k $! f x z
  foldl :: (b -> Element t -> b) -> b -> t -> b
  foldl' :: (b -> Element t -> b) -> b -> t -> b
  foldr1 :: (Element t -> Element t -> Element t) -> t -> Element t
  foldr1 f xs =
#if __GLASGOW_HASKELL__ >= 800
      fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
                (foldr mf Nothing xs)
#else
      fromMaybe (error "foldr1: empty structure")
                (foldr mf Nothing xs)
#endif
    where
      mf x m = Just (case m of
                       Nothing -> x
                       Just y  -> f x y)
  foldl1 :: (Element t -> Element t -> Element t) -> t -> Element t
  foldl1 f xs =
#if __GLASGOW_HASKELL__ >= 800
      fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                (foldl mf Nothing xs)
#else
      fromMaybe (error "foldl1: empty structure")
                (foldl mf Nothing xs)
#endif
    where
      mf m y = Just (case m of
                       Nothing -> y
                       Just x  -> f x y)

  length :: t -> Int

  elem :: Eq (Element t) => Element t -> t -> Bool

  maximum :: Ord (Element t) => t -> Element t
  minimum :: Ord (Element t) => t -> Element t

  all :: (Element t -> Bool) -> t -> Bool
  all p = getAll #. foldMap (All #. p)
  any :: (Element t -> Bool) -> t -> Bool
  any p = getAny #. foldMap (Any #. p)

  and :: (Element t ~ Bool) => t -> Bool
  and = getAll #. foldMap All
  or :: (Element t ~ Bool) => t -> Bool
  or = getAny #. foldMap Any

  find :: (Element t -> Bool) -> t -> Maybe (Element t)
  find p = getFirst . foldMap (\ x -> First (if p x then Just x else Nothing))

  head :: t -> Maybe (Element t)
  head = foldr (\x _ -> Just x) Nothing
  {-# INLINE head #-}

instance {-# OVERLAPPABLE #-} Foldable f => NontrivialContainer (f a) where
  foldMap = F.foldMap
  {-# INLINE foldMap #-}
  fold = F.fold
  {-# INLINE fold #-}
  foldr = F.foldr
  {-# INLINE foldr #-}
  foldr' = F.foldr'
  {-# INLINE foldr' #-}
  foldl = F.foldl
  {-# INLINE foldl #-}
  foldl' = F.foldl'
  {-# INLINE foldl' #-}
  foldr1 = F.foldr1
  {-# INLINE foldr1 #-}
  foldl1 = F.foldl1
  {-# INLINE foldl1 #-}
  length = F.length
  {-# INLINE length #-}
  elem = F.elem
  {-# INLINE elem #-}
  maximum = F.maximum
  {-# INLINE maximum #-}
  minimum = F.minimum
  {-# INLINE minimum #-}
  all = F.all
  {-# INLINE all #-}
  any = F.any
  {-# INLINE any #-}
  and = F.and
  {-# INLINE and #-}
  or = F.or
  {-# INLINE or #-}
  find = F.find
  {-# INLINE find #-}

instance NontrivialContainer T.Text where
  foldr = T.foldr
  {-# INLINE foldr #-}
  foldl = T.foldl
  {-# INLINE foldl #-}
  foldl' = T.foldl'
  {-# INLINE foldl' #-}
  foldr1 = T.foldr1
  {-# INLINE foldr1 #-}
  foldl1 = T.foldl1
  {-# INLINE foldl1 #-}
  length = T.length
  {-# INLINE length #-}
  elem c = T.isInfixOf (T.singleton c)  -- there are rewrite rules for this
  {-# INLINE elem #-}
  maximum = T.maximum
  {-# INLINE maximum #-}
  minimum = T.minimum
  {-# INLINE minimum #-}
  all = T.all
  {-# INLINE all #-}
  any = T.any
  {-# INLINE any #-}
  find = T.find
  {-# INLINE find #-}
  head = fmap fst . T.uncons
  {-# INLINE head #-}

instance NontrivialContainer TL.Text where
  foldr = TL.foldr
  {-# INLINE foldr #-}
  foldl = TL.foldl
  {-# INLINE foldl #-}
  foldl' = TL.foldl'
  {-# INLINE foldl' #-}
  foldr1 = TL.foldr1
  {-# INLINE foldr1 #-}
  foldl1 = TL.foldl1
  {-# INLINE foldl1 #-}
  length = fromIntegral . TL.length
  {-# INLINE length #-}
  -- will be okay thanks to rewrite rules
  elem c s = TL.isInfixOf (TL.singleton c) s
  {-# INLINE elem #-}
  maximum = TL.maximum
  {-# INLINE maximum #-}
  minimum = TL.minimum
  {-# INLINE minimum #-}
  all = TL.all
  {-# INLINE all #-}
  any = TL.any
  {-# INLINE any #-}
  find = TL.find
  {-# INLINE find #-}
  head = fmap fst . TL.uncons
  {-# INLINE head #-}

instance NontrivialContainer BS.ByteString where
  foldr = BS.foldr
  {-# INLINE foldr #-}
  foldl = BS.foldl
  {-# INLINE foldl #-}
  foldl' = BS.foldl'
  {-# INLINE foldl' #-}
  foldr1 = BS.foldr1
  {-# INLINE foldr1 #-}
  foldl1 = BS.foldl1
  {-# INLINE foldl1 #-}
  length = BS.length
  {-# INLINE length #-}
  elem = BS.elem
  {-# INLINE elem #-}
  maximum = BS.maximum
  {-# INLINE maximum #-}
  minimum = BS.minimum
  {-# INLINE minimum #-}
  all = BS.all
  {-# INLINE all #-}
  any = BS.any
  {-# INLINE any #-}
  find = BS.find
  {-# INLINE find #-}
  head = fmap fst . BS.uncons
  {-# INLINE head #-}

instance NontrivialContainer BSL.ByteString where
  foldr = BSL.foldr
  {-# INLINE foldr #-}
  foldl = BSL.foldl
  {-# INLINE foldl #-}
  foldl' = BSL.foldl'
  {-# INLINE foldl' #-}
  foldr1 = BSL.foldr1
  {-# INLINE foldr1 #-}
  foldl1 = BSL.foldl1
  {-# INLINE foldl1 #-}
  length = fromIntegral . BSL.length
  {-# INLINE length #-}
  elem = BSL.elem
  {-# INLINE elem #-}
  maximum = BSL.maximum
  {-# INLINE maximum #-}
  minimum = BSL.minimum
  {-# INLINE minimum #-}
  all = BSL.all
  {-# INLINE all #-}
  any = BSL.any
  {-# INLINE any #-}
  find = BSL.find
  {-# INLINE find #-}
  head = fmap fst . BSL.uncons
  {-# INLINE head #-}

instance NontrivialContainer IS.IntSet where
  foldr = IS.foldr
  {-# INLINE foldr #-}
  foldl = IS.foldl
  {-# INLINE foldl #-}
  foldl' = IS.foldl'
  {-# INLINE foldl' #-}
  length = IS.size
  {-# INLINE length #-}
  elem = IS.member
  {-# INLINE elem #-}
  maximum = IS.findMax
  {-# INLINE maximum #-}
  minimum = IS.findMin
  {-# INLINE minimum #-}
  head = fmap fst . IS.minView
  {-# INLINE head #-}

----------------------------------------------------------------------------
-- Derivative functions
----------------------------------------------------------------------------

-- | Stricter version of 'Prelude.sum'.
--
-- >>> sum [1..10]
-- 55
-- >>> sum (Just 3)
-- <interactive>:43:1: error:
--     • Do not use 'Foldable' methods on Maybe
--     • In the expression: sum (Just 3)
--       In an equation for ‘it’: it = sum (Just 3)
sum :: (NontrivialContainer t, Num (Element t)) => t -> Element t
sum = foldl' (+) 0

-- | Stricter version of 'Prelude.product'.
--
-- >>> product [1..10]
-- 3628800
-- >>> product (Right 3)
-- <interactive>:45:1: error:
--     • Do not use 'Foldable' methods on Either
--     • In the expression: product (Right 3)
--       In an equation for ‘it’: it = product (Right 3)
product :: (NontrivialContainer t, Num (Element t)) => t -> Element t
product = foldl' (*) 1

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.traverse_'.
traverse_
    :: (NontrivialContainer t, Applicative f)
    => (Element t -> f b) -> t -> f ()
traverse_ f = foldr ((*>) . f) pass

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.for_'.
for_
    :: (NontrivialContainer t, Applicative f)
    => t -> (Element t -> f b) -> f ()
for_ = flip traverse_
{-# INLINE for_ #-}

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.mapM_'.
mapM_
    :: (NontrivialContainer t, Monad m)
    => (Element t -> m b) -> t -> m ()
mapM_ f= foldr ((>>) . f) pass

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.forM_'.
forM_
    :: (NontrivialContainer t, Monad m)
    => t -> (Element t -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.sequenceA_'.
sequenceA_
    :: (NontrivialContainer t, Applicative f, Element t ~ f a)
    => t -> f ()
sequenceA_ = foldr (*>) pass

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.sequence_'.
sequence_
    :: (NontrivialContainer t, Monad m, Element t ~ m a)
    => t -> m ()
sequence_ = foldr (>>) pass

-- | Constrained to 'NonTrivialContainer' version of 'Data.Foldable.asum'.
asum
    :: (NontrivialContainer t, Alternative f, Element t ~ f a)
    => t -> f a
asum = foldr (<|>) empty
{-# INLINE asum #-}

----------------------------------------------------------------------------
-- Disallowed instances
----------------------------------------------------------------------------

#define DISALLOW_CONTAINER_8(t, z) \
    instance TypeError (Text "Do not use 'Foldable' methods on " :<>: Text z) => \
      Container (t) where { \
        toList = undefined; \
        null = undefined; } \

#define DISALLOW_NONTRIVIAL_CONTAINER_8(t, z) \
    instance TypeError (Text "Do not use 'Foldable' methods on " :<>: Text z) => \
      NontrivialContainer (t) where { \
        foldr = undefined; \
        foldl = undefined; \
        foldl' = undefined; \
        length = undefined; \
        elem = undefined; \
        maximum = undefined; \
        minimum = undefined; } \

#define DISALLOW_CONTAINER_7(t) \
    instance ForbiddenFoldable (t) => Container (t) where { \
        toList = undefined; \
        null = undefined; } \

#define DISALLOW_NONTRIVIAL_CONTAINER_7(t) \
    instance ForbiddenFoldable (t) => NontrivialContainer (t) where { \
        foldr = undefined; \
        foldl = undefined; \
        foldl' = undefined; \
        length = undefined; \
        elem = undefined; \
        maximum = undefined; \
        minimum = undefined; } \

#if __GLASGOW_HASKELL__ >= 800
DISALLOW_CONTAINER_8((a, b),"tuples")
DISALLOW_NONTRIVIAL_CONTAINER_8((a, b),"tuples")
DISALLOW_NONTRIVIAL_CONTAINER_8(Maybe a,"Maybe")
DISALLOW_NONTRIVIAL_CONTAINER_8(Identity a,"Identity")
DISALLOW_NONTRIVIAL_CONTAINER_8(Either a b,"Either")
#else
class ForbiddenFoldable a
DISALLOW_CONTAINER_7((a, b))
DISALLOW_NONTRIVIAL_CONTAINER_7((a, b))
DISALLOW_NONTRIVIAL_CONTAINER_7(Maybe a)
DISALLOW_NONTRIVIAL_CONTAINER_7(Identity a)
DISALLOW_NONTRIVIAL_CONTAINER_7(Either a b)
#endif

----------------------------------------------------------------------------
-- One
----------------------------------------------------------------------------

-- | Type class for types that can be created from one element. @singleton@
-- is lone name for this function. Also constructions of different type differ:
-- @:[]@ for lists, two arguments for Maps. Also some data types are monomorphic.
--
-- >>> one True :: [Bool]
-- [True]
-- >>> one 'a' :: Text
-- "a"
-- >>> one (3, "hello") :: HashMap Int String
-- fromList [(3,"hello")]
class One x where
    type OneItem x
    -- | Create a list, map, 'Text', etc from a single element.
    one :: OneItem x -> x

-- Lists

instance One [a] where
    type OneItem [a] = a
    one = (:[])
    {-# INLINE one #-}

#if ( __GLASGOW_HASKELL__ >= 800 )
instance One (NE.NonEmpty a) where
    type OneItem (NE.NonEmpty a) = a
    one = (NE.:|[])
    {-# INLINE one #-}
#endif

instance One (SEQ.Seq a) where
    type OneItem (SEQ.Seq a) = a
    one = (SEQ.empty SEQ.|>)
    {-# INLINE one #-}

-- Monomorphic sequences

instance One T.Text where
    type OneItem T.Text = Char
    one = T.singleton
    {-# INLINE one #-}

instance One TL.Text where
    type OneItem TL.Text = Char
    one = TL.singleton
    {-# INLINE one #-}

instance One BS.ByteString where
    type OneItem BS.ByteString = Word8
    one = BS.singleton
    {-# INLINE one #-}

instance One BSL.ByteString where
    type OneItem BSL.ByteString = Word8
    one = BSL.singleton
    {-# INLINE one #-}

-- Maps

instance One (M.Map k v) where
    type OneItem (M.Map k v) = (k, v)
    one = uncurry M.singleton
    {-# INLINE one #-}

instance Hashable k => One (HM.HashMap k v) where
    type OneItem (HM.HashMap k v) = (k, v)
    one = uncurry HM.singleton
    {-# INLINE one #-}

instance One (IM.IntMap v) where
    type OneItem (IM.IntMap v) = (Int, v)
    one = uncurry IM.singleton
    {-# INLINE one #-}

-- Sets

instance One (S.Set v) where
    type OneItem (S.Set v) = v
    one = S.singleton
    {-# INLINE one #-}

instance Hashable v => One (HS.HashSet v) where
    type OneItem (HS.HashSet v) = v
    one = HS.singleton
    {-# INLINE one #-}

instance One IS.IntSet where
    type OneItem IS.IntSet = Int
    one = IS.singleton
    {-# INLINE one #-}

-- Vectors

instance One (V.Vector a) where
    type OneItem (V.Vector a) = a
    one = V.singleton
    {-# INLINE one #-}

instance VU.Unbox a => One (VU.Vector a) where
    type OneItem (VU.Vector a) = a
    one = VU.singleton
    {-# INLINE one #-}

instance VP.Prim a => One (VP.Vector a) where
    type OneItem (VP.Vector a) = a
    one = VP.singleton
    {-# INLINE one #-}

instance VS.Storable a => One (VS.Vector a) where
    type OneItem (VS.Vector a) = a
    one = VS.singleton
    {-# INLINE one #-}

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
