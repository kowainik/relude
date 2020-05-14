{-# LANGUAGE Trustworthy  #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

Contains implementation of polymorphic type classes for data types 'Set' and
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

@since 0.1.0
-}
class StaticMap t where
    type Key t :: Type
    type Val t :: Type

    size   :: t -> Int
    lookup :: Key t -> t -> Maybe (Val t)
    member :: Key t -> t -> Bool

{- |

@since 0.1.0
-}
instance Ord k => StaticMap (Map k v) where
    type Key (Map k v) = k
    type Val (Map k v) = v

    size = M.size
    {-# INLINE size #-}
    lookup = M.lookup
    {-# INLINE lookup #-}
    member = M.member
    {-# INLINE member #-}

{- |

@since 0.1.0
-}
instance (Eq k, Hashable k) => StaticMap (HashMap k v) where
    type Key (HashMap k v) = k
    type Val (HashMap k v) = v

    size = HM.size
    {-# INLINE size #-}
    lookup = HM.lookup
    {-# INLINE lookup #-}
    member = HM.member
    {-# INLINE member #-}

{- |

@since 0.1.0
-}
instance StaticMap (IntMap v) where
    type Key (IntMap v) = Int
    type Val (IntMap v) = v

    size = IM.size
    {-# INLINE size #-}
    lookup = IM.lookup
    {-# INLINE lookup #-}
    member = IM.member
    {-# INLINE member #-}

{- |

@since 0.1.0
-}
instance Ord a => StaticMap (Set a) where
    type Key (Set a) = a
    type Val (Set a) = a

    size = S.size
    {-# INLINE size #-}
    member = S.member
    {-# INLINE member #-}
    lookup k m = guard (member k m) $> k
    {-# INLINE lookup #-}

{- |

@since 0.1.0
-}
instance (Eq a, Hashable a) => StaticMap (HashSet a) where
    type Key (HashSet a) = a
    type Val (HashSet a) = a

    size = HS.size
    {-# INLINE size #-}
    member = HS.member
    {-# INLINE member #-}
    lookup k m = guard (member k m) $> k
    {-# INLINE lookup #-}

{- |

@since 0.1.0
-}
instance StaticMap IntSet where
    type Key IntSet = Int
    type Val IntSet = Int

    size = IS.size
    {-# INLINE size #-}
    member = IS.member
    {-# INLINE member #-}
    lookup k m = guard (member k m) $> k
    {-# INLINE lookup #-}

{- | Operator version of 'lookup' function.

>>> let myHashMap = HashMap.fromList [('a', "xxx"), ('b', "yyy")]
>>> myHashMap !? 'b'
Just "yyy"

>>> myHashMap !? 'd'
Nothing

@since 0.1.0
-}
infixl 9 !?
(!?) :: StaticMap t => t -> Key t -> Maybe (Val t)
(!?) m k = lookup k m
{-# INLINE (!?) #-}

{- | Inverse of 'member' function.

>>> let myHashMap = HashMap.fromList [('a', "xxx"), ('b', "yyy")]
>>> notMember 'b' myHashMap
False

>>> notMember 'c' myHashMap
True

@since 0.1.0
-}
notMember :: StaticMap t => Key t -> t -> Bool
notMember k = not . member k
{-# INLINE notMember #-}

{- | Return the value to which the specified key is mapped, or the default value
if this map contains no mapping for the key.

>>> let myHashMap = HashMap.fromList [('a', "xxx"), ('b', "yyy")]
>>> lookupDefault "zzz" 'b' myHashMap
"yyy"

>>> lookupDefault "zzz" 'c' myHashMap
"zzz"

@since 0.1.0
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

@since 0.1.0
-}
class StaticMap t => DynamicMap t where
    -- insertions
    insert     :: Key t -> Val t -> t -> t
    insertWith :: (Val t -> Val t -> Val t) -> Key t -> Val t -> t -> t

    -- deletions
    delete :: Key t -> t -> t
    alter :: (Maybe (Val t) -> Maybe (Val t)) -> Key t -> t -> t

{- |

@since 0.1.0
-}
instance Ord k => DynamicMap (Map k v) where
    insert     = M.insert
    {-# INLINE insert #-}
    insertWith = M.insertWith
    {-# INLINE insertWith #-}
    delete     = M.delete
    {-# INLINE delete #-}
    alter      = M.alter
    {-# INLINE alter #-}

{- |

@since 0.1.0
-}
instance (Eq k, Hashable k) => DynamicMap (HashMap k v) where
    insert     = HM.insert
    {-# INLINE insert #-}
    insertWith = HM.insertWith
    {-# INLINE insertWith #-}
    delete     = HM.delete
    {-# INLINE delete #-}
    alter      = HM.alter
    {-# INLINE alter #-}

{- |

@since 0.1.0
-}
instance DynamicMap (IntMap v) where
    insert     = IM.insert
    {-# INLINE insert #-}
    insertWith = IM.insertWith
    {-# INLINE insertWith #-}
    delete     = IM.delete
    {-# INLINE delete #-}
    alter      = IM.alter
    {-# INLINE alter #-}

----------------------------------------------------------------------------
-- ToPairs
----------------------------------------------------------------------------

-- $setup
-- >>> import qualified Data.HashMap.Strict as HashMap

{- | Converts the structure to the list of the key-value pairs.

>>> toPairs (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
[('a',"xxx"),('b',"yyy")]

@since 0.1.0
-}
toPairs :: (IsList t, Item t ~ (a, b)) => t -> [(a, b)]
toPairs = toList

{- | Converts the structure to the list of the keys.

>>> keys (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
"ab"

@since 0.1.0
-}
keys :: (IsList t, Item t ~ (a, b)) => t -> [a]
keys = map fst . toList

{- | Converts the structure to the list of the values.

>>> elems (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
["xxx","yyy"]

@since 0.1.0
-}
elems :: (IsList t, Item t ~ (a, b)) => t -> [b]
elems = map snd . toList
