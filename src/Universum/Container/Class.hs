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

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

{- | Reimagined approach for 'Foldable' type hierarchy. Forbids usages
of 'length' function and similar over 'Maybe' and other potentially unsafe
data types. It was proposed to use @-XTypeApplication@ for such cases.
But this approach is not robust enough because programmers are human and can
easily forget to do this. For discussion see this topic:
<https://www.reddit.com/r/haskell/comments/60r9hu/proposal_suggest_explicit_type_application_for/ Suggest explicit type application for Foldable length and friends>
-}

module Universum.Container.Class
       ( -- * Container-like classes and methods
         ToPairs   (..)
       , One(..)
       ) where

import Prelude hiding (print)

import Universum.Base (Word8)
import Universum.Container.Reexport (HashMap, HashSet, Hashable, IntMap, IntSet, Map, Set, Vector)

#if ( __GLASGOW_HASKELL__ >= 800 )
import qualified Data.List.NonEmpty as NE
#endif

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

-- $setup
-- >>> import Universum.Base (even)
-- >>> import Universum.Bool (when)
-- >>> import Universum.Print (print, putTextLn)
-- >>> import Universum.String (Text)
-- >>> import qualified Data.HashMap.Strict as HashMap

----------------------------------------------------------------------------
-- ToPairs
----------------------------------------------------------------------------

{- | Type class for data types that can be converted to List of Pairs.
 You can define 'ToPairs' by just defining 'toPairs' function.

 But the following laws should be met:

@
'toPairs' m ≡ 'zip' ('keys' m) ('elems' m)
'keys'      ≡ 'map' 'fst' . 'toPairs'
'elems'     ≡ 'map' 'snd' . 'toPairs'
@

-}
class ToPairs t where
    {-# MINIMAL toPairs #-}
    -- | Type of keys of the mapping.
    type Key t :: *
    -- | Type of value of the mapping.
    type Val t :: *

    -- | Converts the structure to the list of the key-value pairs.
    -- >>> toPairs (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
    -- [('a',"xxx"),('b',"yyy")]
    toPairs :: t -> [(Key t, Val t)]

    -- | Converts the structure to the list of the keys.
    --
    -- >>> keys (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
    -- "ab"
    keys :: t -> [Key t]
    keys = map fst . toPairs
    {-# INLINE keys #-}

    -- | Converts the structure to the list of the values.
    --
    -- >>> elems (HashMap.fromList [('a', "xxx"), ('b', "yyy")])
    -- ["xxx","yyy"]
    elems :: t -> [Val t]
    elems = map snd . toPairs
    {-# INLINE elems #-}

-- Instances

instance ToPairs (HashMap k v) where
    type Key (HashMap k v) = k
    type Val (HashMap k v) = v
    toPairs = HM.toList
    {-# INLINE toPairs #-}
    keys    = HM.keys
    {-# INLINE keys #-}
    elems   = HM.elems
    {-# INLINE elems #-}

instance ToPairs (IntMap v) where
    type Key (IntMap v) = Int
    type Val (IntMap v) = v
    toPairs = IM.toList
    {-# INLINE toPairs #-}
    keys    = IM.keys
    {-# INLINE keys #-}
    elems   = IM.elems
    {-# INLINE elems #-}

instance ToPairs (Map k v) where
    type Key (Map k v) = k
    type Val (Map k v) = v
    toPairs = M.toList
    {-# INLINE toPairs #-}
    keys    = M.keys
    {-# INLINE keys #-}
    elems   = M.elems
    {-# INLINE elems #-}

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
