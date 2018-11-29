{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Typeclass for creating structures from singleton element.
-}

module Relude.Container.One
       ( One (..)
       ) where

import Relude.Base (Char, Word8)
import Relude.Container.Reexport (HashMap, HashSet, Hashable, IntMap, IntSet, Map, Set, uncurry)
import Relude.Numeric (Int)

import qualified Data.List.NonEmpty as NE

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

-- $setup
-- >>> import Relude.Base (String)
-- >>> import Relude.Bool (Bool (..))
-- >>> import Relude.Numeric (Int)
-- >>> import Relude.String (Text)
-- >>> import qualified Data.HashMap.Strict as HashMap

{- | Typeclass for data types that can be created from one element.

>>> one True :: [Bool]
[True]
>>> one 'a' :: Text
"a"
>>> one (3, "hello") :: HashMap Int String
fromList [(3,"hello")]
-}
class One x where
    -- | Type of single element of the structure.
    type OneItem x
    -- | Create a list, map, 'Text', etc from a single element.
    one :: OneItem x -> x

-- Lists

instance One [a] where
    type OneItem [a] = a
    one = (:[])
    {-# INLINE one #-}

instance One (NE.NonEmpty a) where
    type OneItem (NE.NonEmpty a) = a
    one = (NE.:|[])
    {-# INLINE one #-}

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

instance One (Map k v) where
    type OneItem (Map k v) = (k, v)
    one = uncurry M.singleton
    {-# INLINE one #-}

instance Hashable k => One (HashMap k v) where
    type OneItem (HashMap k v) = (k, v)
    one = uncurry HM.singleton
    {-# INLINE one #-}

instance One (IntMap v) where
    type OneItem (IntMap v) = (Int, v)
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
