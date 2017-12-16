{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE Trustworthy             #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Reimagined approach for 'Foldable' type hierarchy. Forbids usages
-- of 'length' function and similar over 'Maybe' and other potentially unsafe
-- data types. It was proposed to use @-XTypeApplication@ for such cases.
-- But this approach is not robust enough because programmers are human and can
-- easily forget to do this. For discussion see this topic:
-- <https://www.reddit.com/r/haskell/comments/60r9hu/proposal_suggest_explicit_type_application_for/ Suggest explicit type application for Foldable length and friends>

module Universum.Container.Class
       ( -- * Foldable-like classes and methods
         ToList    (..)
       , Container (..)

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

import Data.Coerce (Coercible, coerce)
import Prelude hiding (all, and, any, elem, foldMap, foldl, foldr, mapM_, notElem, or, product,
                sequence_, sum)

import Universum.Applicative (Alternative (..), Const, ZipList, pass)
import Universum.Base (Constraint, Word8)
import Universum.Container.Reexport (HashMap, HashSet, Hashable, IntMap, IntSet, Map, Seq, Set,
                                     Vector)
import Universum.Functor (Identity)
import Universum.Monad.Reexport (fromMaybe)
import Universum.Monoid (All (..), Any (..), Dual, First (..), Last, Product, Sum)

#if __GLASGOW_HASKELL__ >= 800
import GHC.Err (errorWithoutStackTrace)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
#endif

#if ( __GLASGOW_HASKELL__ >= 800 )
import qualified Data.List.NonEmpty as NE
import Universum.Monoid (NonEmpty)
#endif

import qualified Data.Foldable as Foldable

import qualified Data.List as List (null)

import qualified Data.Sequence as SEQ

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as Set

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

----------------------------------------------------------------------------
-- Containers (e.g. tuples aren't containers)
----------------------------------------------------------------------------

-- | Default implementation of 'Element' associated type family.
type family ElementDefault (t :: *) :: * where
    ElementDefault (f a) = a

-- | Type class for data types that can be converted to List.
-- Fully compatible with 'Foldable'.
-- Contains very small and safe subset of 'Foldable' functions.
--
-- You can define 'Tolist' by just defining 'toList' function.
-- But the following law should be met:
--
-- @'null' ≡ 'List.null' . 'toList'@
--
class ToList t where
    -- | Type of element for some container. Implemented as an asscociated type family because
    -- some containers are monomorphic over element type (like 'T.Text', 'IntSet', etc.)
    -- so we can't implement nice interface using old higher-kinded types
    -- approach. Implementing this as an associated type family instead of
    -- top-level family gives you more control over element types.
    type Element t :: *
    type Element t = ElementDefault t

    -- | Convert container to list of elements.
    --
    -- >>> toList (Just True)
    -- [True]
    -- >>> toList @Text "aba"
    -- "aba"
    -- >>> :t toList @Text "aba"
    -- toList @Text "aba" :: [Char]
    toList :: t -> [Element t]
    default toList :: (Foldable f, t ~ f a, Element t ~ a) => t -> [Element t]
    toList = Foldable.toList
    {-# INLINE toList #-}

    -- | Checks whether container is empty.
    --
    -- >>> null @Text ""
    -- True
    -- >>> null @Text "aba"
    -- False
    null :: t -> Bool
    null = List.null . toList
    {-# INLINE null #-}

----------------------------------------------------------------------------
-- Instances for monomorphic containers
----------------------------------------------------------------------------

instance ToList T.Text where
    type Element T.Text = Char
    toList = T.unpack
    {-# INLINE toList #-}
    null = T.null
    {-# INLINE null #-}

instance ToList TL.Text where
    type Element TL.Text = Char
    toList = TL.unpack
    {-# INLINE toList #-}
    null = TL.null
    {-# INLINE null #-}

instance ToList BS.ByteString where
    type Element BS.ByteString = Word8
    toList = BS.unpack
    {-# INLINE toList #-}
    null = BS.null
    {-# INLINE null #-}

instance ToList BSL.ByteString where
    type Element BSL.ByteString = Word8
    toList = BSL.unpack
    {-# INLINE toList #-}
    null = BSL.null
    {-# INLINE null #-}

instance ToList IntSet where
    type Element IntSet = Int
    toList = IS.toList
    {-# INLINE toList #-}
    null = IS.null
    {-# INLINE null #-}

----------------------------------------------------------------------------
-- Boilerplate instances (duplicate Foldable)
----------------------------------------------------------------------------

-- Basic types
instance ToList [a]
instance ToList (Maybe a)
instance ToList (Either a b)
instance ToList (Identity a)
instance ToList (Const a b)

#if __GLASGOW_HASKELL__ >= 800
-- Algebraic types
instance ToList (Dual a)
instance ToList (First a)
instance ToList (Last a)
instance ToList (Product a)
instance ToList (Sum a)
instance ToList (NonEmpty a)
instance ToList (ZipList a)
#endif

-- Containers
instance ToList (HashMap k v)
instance ToList (HashSet v)
instance ToList (IntMap v)
instance ToList (Map k v)
instance ToList (Set v)
instance ToList (Seq a)
instance ToList (Vector a)

----------------------------------------------------------------------------
-- Additional operations that don't make much sense for e.g. Maybe
----------------------------------------------------------------------------

-- | A class for 'ToList's that aren't trivial like 'Maybe' (e.g. can hold
-- more than one value)
class ToList t => Container t where
    -- | Constraint for elements. This can be used to implement more efficient
    -- implementation of some methods.
    type ElementConstraint t :: * -> Constraint
    type ElementConstraint t = Eq

    foldr :: (Element t -> b -> b) -> b -> t -> b
    default foldr :: (Foldable f, t ~ f a, Element t ~ a) => (Element t -> b -> b) -> b -> t -> b
    foldr = Foldable.foldr
    {-# INLINE foldr #-}

    foldl :: (b -> Element t -> b) -> b -> t -> b
    default foldl :: (Foldable f, t ~ f a, Element t ~ a) => (b -> Element t -> b) -> b -> t -> b
    foldl = Foldable.foldl
    {-# INLINE foldl #-}

    foldl' :: (b -> Element t -> b) -> b -> t -> b
    default foldl' :: (Foldable f, t ~ f a, Element t ~ a) => (b -> Element t -> b) -> b -> t -> b
    foldl' = Foldable.foldl'
    {-# INLINE foldl' #-}

    length :: t -> Int
    default length :: (Foldable f, t ~ f a, Element t ~ a) => t -> Int
    length = Foldable.length
    {-# INLINE length #-}

    elem :: ElementConstraint t (Element t) => Element t -> t -> Bool
    default elem :: ( Foldable f
                    , t ~ f a
                    , Element t ~ a
                    , ElementConstraint t ~ Eq
                    , ElementConstraint t (Element t)
                    ) => Element t -> t -> Bool
    elem = Foldable.elem
    {-# INLINE elem #-}

    maximum :: Ord (Element t) => t -> Element t
    default maximum :: (Foldable f, t ~ f a, Element t ~ a, Ord (Element t)) => t -> Element t
    maximum = Foldable.maximum
    {-# INLINE maximum #-}

    minimum :: Ord (Element t) => t -> Element t
    default minimum :: (Foldable f, t ~ f a, Element t ~ a, Ord (Element t)) => t -> Element t
    minimum = Foldable.minimum
    {-# INLINE minimum #-}

    foldMap :: Monoid m => (Element t -> m) -> t -> m
    foldMap f = foldr (mappend . f) mempty
    {-# INLINE foldMap #-}

    fold :: Monoid (Element t) => t -> Element t
    fold = foldMap id
    {-# INLINE fold #-}

    foldr' :: (Element t -> b -> b) -> b -> t -> b
    foldr' f z0 xs = foldl f' id xs z0
      where f' k x z = k $! f x z
    {-# INLINE foldr' #-}

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
    {-# INLINE foldr1 #-}

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
    {-# INLINE foldl1 #-}

    notElem :: ElementConstraint t (Element t) => Element t -> t -> Bool
    notElem x = not . elem x
    {-# INLINE notElem #-}

    all :: (Element t -> Bool) -> t -> Bool
    all p = getAll #. foldMap (All #. p)
    any :: (Element t -> Bool) -> t -> Bool
    any p = getAny #. foldMap (Any #. p)
    {-# INLINE all #-}
    {-# INLINE any #-}

    and :: (Element t ~ Bool) => t -> Bool
    and = getAll #. foldMap All
    or :: (Element t ~ Bool) => t -> Bool
    or = getAny #. foldMap Any
    {-# INLINE and #-}
    {-# INLINE or #-}

    find :: (Element t -> Bool) -> t -> Maybe (Element t)
    find p = getFirst . foldMap (\ x -> First (if p x then Just x else Nothing))
    {-# INLINE find #-}

    safeHead :: t -> Maybe (Element t)
    safeHead = foldr (\x _ -> Just x) Nothing
    {-# INLINE safeHead #-}

----------------------------------------------------------------------------
-- Instances for monomorphic containers
----------------------------------------------------------------------------

instance Container T.Text where
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
    safeHead = fmap fst . T.uncons
    {-# INLINE safeHead #-}

instance Container TL.Text where
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
    safeHead = fmap fst . TL.uncons
    {-# INLINE safeHead #-}

instance Container BS.ByteString where
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
    notElem = BS.notElem
    {-# INLINE notElem #-}
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
    safeHead = fmap fst . BS.uncons
    {-# INLINE safeHead #-}

instance Container BSL.ByteString where
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
    notElem = BSL.notElem
    {-# INLINE notElem #-}
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
    safeHead = fmap fst . BSL.uncons
    {-# INLINE safeHead #-}

instance Container IntSet where
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
    safeHead = fmap fst . IS.minView
    {-# INLINE safeHead #-}

----------------------------------------------------------------------------
-- Efficient instances
----------------------------------------------------------------------------

instance Container (Set v) where
    type ElementConstraint (Set v) = Ord
    elem = Set.member
    {-# INLINE elem #-}
    notElem = Set.notMember
    {-# INLINE notElem #-}

class (Eq a, Hashable a) => CanHash a
instance (Eq a, Hashable a) => CanHash a

instance Container (HashSet v) where
    type ElementConstraint (HashSet v) = CanHash
    elem = HashSet.member
    {-# INLINE elem #-}

----------------------------------------------------------------------------
-- Boilerplate instances (duplicate Foldable)
----------------------------------------------------------------------------

-- Basic types
instance Container [a]
instance Container (Const a b)

#if __GLASGOW_HASKELL__ >= 800
-- Algebraic types
instance Container (Dual a)
instance Container (First a)
instance Container (Last a)
instance Container (Product a)
instance Container (Sum a)
instance Container (NonEmpty a)
instance Container (ZipList a)
#endif

-- Containers
instance Container (HashMap k v)
instance Container (IntMap v)
instance Container (Map k v)
instance Container (Seq a)
instance Container (Vector a)

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
sum :: (Container t, Num (Element t)) => t -> Element t
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
product :: (Container t, Num (Element t)) => t -> Element t
product = foldl' (*) 1

-- | Constrained to 'Container' version of 'Data.Foldable.traverse_'.
traverse_
    :: (Container t, Applicative f)
    => (Element t -> f b) -> t -> f ()
traverse_ f = foldr ((*>) . f) pass

-- | Constrained to 'Container' version of 'Data.Foldable.for_'.
for_
    :: (Container t, Applicative f)
    => t -> (Element t -> f b) -> f ()
for_ = flip traverse_
{-# INLINE for_ #-}

-- | Constrained to 'Container' version of 'Data.Foldable.mapM_'.
mapM_
    :: (Container t, Monad m)
    => (Element t -> m b) -> t -> m ()
mapM_ f= foldr ((>>) . f) pass

-- | Constrained to 'Container' version of 'Data.Foldable.forM_'.
forM_
    :: (Container t, Monad m)
    => t -> (Element t -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

-- | Constrained to 'Container' version of 'Data.Foldable.sequenceA_'.
sequenceA_
    :: (Container t, Applicative f, Element t ~ f a)
    => t -> f ()
sequenceA_ = foldr (*>) pass

-- | Constrained to 'Container' version of 'Data.Foldable.sequence_'.
sequence_
    :: (Container t, Monad m, Element t ~ m a)
    => t -> m ()
sequence_ = foldr (>>) pass

-- | Constrained to 'Container' version of 'Data.Foldable.asum'.
asum
    :: (Container t, Alternative f, Element t ~ f a)
    => t -> f a
asum = foldr (<|>) empty
{-# INLINE asum #-}

----------------------------------------------------------------------------
-- Disallowed instances
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 800
type family DisallowInstance (z :: Symbol) :: ErrorMessage where
    DisallowInstance z  = Text "Do not use 'Foldable' methods on " :<>: Text z
        :$$: Text "Suggestions:"
        :$$: Text "    Instead of"
        :$$: Text "        for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()"
        :$$: Text "    use"
        :$$: Text "        whenJust  :: Applicative f => Maybe a    -> (a -> f ()) -> f ()"
        :$$: Text "        whenRight :: Applicative f => Either l r -> (r -> f ()) -> f ()"
        :$$: Text ""
        :$$: Text "    Instead of"
        :$$: Text "        fold :: (Foldable t, Monoid m) => t m -> m"
        :$$: Text "    use"
        :$$: Text "        maybeToMonoid :: Monoid m => Maybe m -> m"
        :$$: Text ""
#endif

#define DISALLOW_TO_LIST_8(t, z) \
    instance TypeError (DisallowInstance z) => \
      ToList (t) where { \
        toList = undefined; \
        null = undefined; } \

#define DISALLOW_CONTAINER_8(t, z) \
    instance TypeError (DisallowInstance z) => \
      Container (t) where { \
        foldr = undefined; \
        foldl = undefined; \
        foldl' = undefined; \
        length = undefined; \
        elem = undefined; \
        maximum = undefined; \
        minimum = undefined; } \

#define DISALLOW_TO_LIST_7(t) \
    instance ForbiddenFoldable (t) => ToList (t) where { \
        toList = undefined; \
        null = undefined; } \

#define DISALLOW_CONTAINER_7(t) \
    instance ForbiddenFoldable (t) => Container (t) where { \
        foldr = undefined; \
        foldl = undefined; \
        foldl' = undefined; \
        length = undefined; \
        elem = undefined; \
        maximum = undefined; \
        minimum = undefined; } \

#if __GLASGOW_HASKELL__ >= 800
DISALLOW_TO_LIST_8((a, b),"tuples")
DISALLOW_CONTAINER_8((a, b),"tuples")
DISALLOW_CONTAINER_8(Maybe a,"Maybe")
DISALLOW_CONTAINER_8(Identity a,"Identity")
DISALLOW_CONTAINER_8(Either a b,"Either")
#else
class ForbiddenFoldable a
DISALLOW_TO_LIST_7((a, b))
DISALLOW_CONTAINER_7((a, b))
DISALLOW_CONTAINER_7(Maybe a)
DISALLOW_CONTAINER_7(Identity a)
DISALLOW_CONTAINER_7(Either a b)
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

instance One (Set v) where
    type OneItem (Set v) = v
    one = Set.singleton
    {-# INLINE one #-}

instance Hashable v => One (HashSet v) where
    type OneItem (HashSet v) = v
    one = HashSet.singleton
    {-# INLINE one #-}

instance One IntSet where
    type OneItem IntSet = Int
    one = IS.singleton
    {-# INLINE one #-}

-- Vectors

instance One (Vector a) where
    type OneItem (Vector a) = a
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
