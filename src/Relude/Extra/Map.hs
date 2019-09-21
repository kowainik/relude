{-# LANGUAGE TypeFamilies #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains implementation of polymorhic type classes for data types 'Set' and
'Map'.
-}

module Relude.Extra.Map
       ( StaticMap (..)
       , DynamicMap (..)
       , (!?)
       , notMember
       , lookupDefault

         -- * To pairs
       , toPairs
       , keys
       , elems
       ) where

import GHC.Exts (IsList (Item, toList))

import Relude.Base (Eq, Ord, Type)
import Relude.Bool (Bool, guard, not)
import Relude.Container.Reexport (HashMap, HashSet, Hashable, IntMap, IntSet, Map, Set, fst, snd)
import Relude.Function ((.))
import Relude.Functor.Reexport (($>))
import Relude.List (map)
import Relude.Monad.Reexport (Maybe (..), fromMaybe)
import Relude.Numeric (Int)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

----------------------------------------------------------------------------
-- Static Map
----------------------------------------------------------------------------

{- | Read-only map or set. Contains polymorphic functions which work for both
sets and maps.
-}
class StaticMap t where
    type Key t :: Type
    type Val t :: Type

    size   :: t -> Int
    lookup :: Key t -> t -> Maybe (Val t)
    member :: Key t -> t -> Bool

instance Ord k => StaticMap (Map k v) where
    type Key (Map k v) = k
    type Val (Map k v) = v

    size   = M.size
    lookup = M.lookup
    member = M.member
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance (Eq k, Hashable k) => StaticMap (HashMap k v) where
    type Key (HashMap k v) = k
    type Val (HashMap k v) = v

    size   = HM.size
    lookup = HM.lookup
    member = HM.member
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance StaticMap (IntMap v) where
    type Key (IntMap v) = Int
    type Val (IntMap v) = v

    size   = IM.size
    lookup = IM.lookup
    member = IM.member
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance Ord a => StaticMap (Set a) where
    type Key (Set a) = a
    type Val (Set a) = a

    size   = S.size
    member = S.member
    lookup k m = guard (member k m) $> k
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance (Eq a, Hashable a) => StaticMap (HashSet a) where
    type Key (HashSet a) = a
    type Val (HashSet a) = a

    size   = HS.size
    member = HS.member
    lookup k m = guard (member k m) $> k
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance StaticMap IntSet where
    type Key IntSet = Int
    type Val IntSet = Int

    size   = IS.size
    member = IS.member
    lookup k m = guard (member k m) $> k
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

-- | Operator version of 'lookup' function.
infixl 9 !?
(!?) :: StaticMap t => t -> Key t -> Maybe (Val t)
(!?) m k = lookup k m
{-# INLINE (!?) #-}

-- | Inverse of 'member' function.
notMember :: StaticMap t => Key t -> t -> Bool
notMember k = not . member k
{-# INLINE notMember #-}

{- | Return the value to which the specified key is mapped, or the default value
if this map contains no mapping for the key.
-}
lookupDefault :: StaticMap t
              => Val t -- ^ Default value to return.
              -> Key t -- ^ Key to search
              -> t     -- ^ Container to search
              -> Val t
lookupDefault def k = fromMaybe def . lookup k
{-# INLINE lookupDefault #-}

----------------------------------------------------------------------------
-- Dynamic Map
----------------------------------------------------------------------------

{- | Modifiable Map.
-}
class StaticMap t => DynamicMap t where
    -- insertions
    insert     :: Key t -> Val t -> t -> t
    insertWith :: (Val t -> Val t -> Val t) -> Key t -> Val t -> t -> t

    -- deletions
    delete :: Key t -> t -> t
    alter :: (Maybe (Val t) -> Maybe (Val t)) -> Key t -> t -> t

instance Ord k => DynamicMap (Map k v) where
    insert     = M.insert
    insertWith = M.insertWith
    delete     = M.delete
    alter      = M.alter

    {-# INLINE insert #-}
    {-# INLINE insertWith #-}
    {-# INLINE delete #-}
    {-# INLINE alter #-}

instance (Eq k, Hashable k) => DynamicMap (HashMap k v) where
    insert     = HM.insert
    insertWith = HM.insertWith
    delete     = HM.delete
    alter      = HM.alter

    {-# INLINE insert #-}
    {-# INLINE insertWith #-}
    {-# INLINE delete #-}
    {-# INLINE alter #-}

instance DynamicMap (IntMap v) where
    insert     = IM.insert
    insertWith = IM.insertWith
    delete     = IM.delete
    alter      = IM.alter

    {-# INLINE insert #-}
    {-# INLINE insertWith #-}
    {-# INLINE delete #-}
    {-# INLINE alter #-}

----------------------------------------------------------------------------
-- ToPairs
----------------------------------------------------------------------------

-- $setup
-- >>> import qualified Data.HashMap.Strict as HashMap

{- | Converts the structure to the list of the key-value pairs.

>>> toPairs (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
[('a',"xxx"),('b',"yyy")]
-}
toPairs :: (IsList t, Item t ~ (a, b)) => t -> [(a, b)]
toPairs = toList

{- | Converts the structure to the list of the keys.

>>> keys (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
"ab"
-}
keys :: (IsList t, Item t ~ (a, b)) => t -> [a]
keys = map fst . toList

{- | Converts the structure to the list of the values.

>>> elems (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
["xxx","yyy"]
-}
elems :: (IsList t, Item t ~ (a, b)) => t -> [b]
elems = map snd . toList
