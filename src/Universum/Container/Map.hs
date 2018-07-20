{-# LANGUAGE TypeFamilies #-}

{- | Contains implememtation of polymorhic type classes for things like 'Set'
and 'Map'.
-}

module Universum.Container.Map
       ( StaticMap (..)
       , DynamicMap (..)
       , (!?)

         -- * To pairs
       , toPairs
       , keys
       , elems
       ) where

import GHC.Exts (IsList (Item, toList))

import Universum.Applicative (pure, (*>))
import Universum.Base (Eq, Int, Ord)
import Universum.Bool (Bool, guard)
import Universum.Container.Reexport (HashMap, HashSet, Hashable, IntMap, IntSet, Map, Set, fst, snd)
import Universum.Function ((.))
import Universum.Functor (map)
import Universum.Monad.Reexport (Maybe (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

----------------------------------------------------------------------------
-- Static Map
----------------------------------------------------------------------------

{- | Read-only map or set.
-}
class StaticMap t where
    type Key t :: *
    type Val t :: *

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
    lookup k m = guard (member k m) *> pure k
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance (Eq a, Hashable a) => StaticMap (HashSet a) where
    type Key (HashSet a) = a
    type Val (HashSet a) = a

    size   = HS.size
    member = HS.member
    lookup k m = guard (member k m) *> pure k
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

instance StaticMap IntSet where
    type Key IntSet = Int
    type Val IntSet = Int

    size   = IS.size
    member = IS.member
    lookup k m = guard (member k m) *> pure k
    {-# INLINE size #-}
    {-# INLINE lookup #-}
    {-# INLINE member #-}

-- | Operator version of 'lookup' function.
infixl 9 !?
(!?) :: StaticMap t => t -> Key t -> Maybe (Val t)
(!?) m k = lookup k m
{-# INLINE (!?) #-}

----------------------------------------------------------------------------
-- Dynamic Map
----------------------------------------------------------------------------

{- | Modifiable Map.
-}
class StaticMap t => DynamicMap t where
    -- insertions
    insert :: Key t -> Val t -> t -> t
    insertWith :: (Val t -> Val t -> Val t) -> Key t -> Val t -> t -> t
    insertWithKey :: (Key t -> Val t -> Val t -> Val t) -> Key t -> Val t -> t -> t

    -- deletions
    delete :: Key t -> t -> t
    alter :: (Maybe (Val t) -> Maybe (Val t)) -> Key t -> t -> t

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
