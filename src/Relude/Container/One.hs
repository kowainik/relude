{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-present Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Typeclass for creating structures from a singleton element. It has three main goals:

1. Give a shorter name for the construction: uses 'one' instead of common @singleton@.
2. Work with monomorphic structures like 'T.Text' or 'IntSet'.
3. Give a clearer and less scary name for cases where you can use 'pure' or @(:[])@.
-}

module Relude.Container.One
       ( One (..)
       ) where

import Relude.Base (Char)
import Relude.Container.Reexport (HashMap, HashSet, Hashable, IntMap, IntSet, Map, Set, uncurry)
import Relude.Numeric (Int, Word8)

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
-- >>> import Relude
-- >>> import qualified Data.IntSet as IntSet
-- >>> import qualified Data.HashMap.Strict as HashMap
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.ByteString as ByteString
-- >>> import qualified Data.Text.Lazy as LText
-- >>> import qualified Data.ByteString.Lazy as LByteString

{- | Typeclass for data types that can be created from one element.

>>> one True :: [Bool]
[True]
>>> one 'a' :: Text
"a"
>>> one (3, "hello") :: HashMap Int String
fromList [(3,"hello")]

__Laws:__

* __@single-size@__: @∀ x . size (one x) ≡ 1@

(where @size@ is a specific function for each container that returns the size of
this container)
-}
class One x where
    -- | Type of single element of the structure.
    type OneItem x
    -- | Create a list, map, 'Text', etc from a single element.
    one :: OneItem x -> x

-- Lists

{- | Allows to create a singleton list. You might prefer function with name 'one'
instead of 'pure' or @(:[])@.

>>> one 42 :: [Int]
[42]

prop> length (one @[Int] x) == 1
-}
instance One [a] where
    type OneItem [a] = a
    one = (:[])
    {-# INLINE one #-}

{- | Allows to create singleton 'NE.NonEmpty' list. You might prefer function with
name 'one' instead of 'pure' or @(:|[])@.

>>> one 42 :: NonEmpty Int
42 :| []

prop> length (one @(NonEmpty Int) x) == 1
-}
instance One (NE.NonEmpty a) where
    type OneItem (NE.NonEmpty a) = a
    one = (NE.:|[])
    {-# INLINE one #-}

{- | Create singleton 'SEQ.Seq'.

>>> one 42 :: Seq Int
fromList [42]

prop> length (one @(Seq Int) x) == 1
-}
instance One (SEQ.Seq a) where
    type OneItem (SEQ.Seq a) = a
    one = SEQ.singleton
    {-# INLINE one #-}

-- Monomorphic sequences

{- | Create singleton strict 'T.Text'.

>>> one 'a' :: Text
"a"

prop> Text.length (one x) == 1
-}
instance One T.Text where
    type OneItem T.Text = Char
    one = T.singleton
    {-# INLINE one #-}

{- | Create singleton lazy 'TL.Text'.

>>> one 'a' :: LText
"a"

prop> LText.length (one x) == 1
-}
instance One TL.Text where
    type OneItem TL.Text = Char
    one = TL.singleton
    {-# INLINE one #-}

{- | Create singleton strict 'BS.ByteString'.

>>> one 97 :: ByteString
"a"

prop> ByteString.length (one x) == 1
-}
instance One BS.ByteString where
    type OneItem BS.ByteString = Word8
    one = BS.singleton
    {-# INLINE one #-}

{- | Create singleton lazy 'BSL.ByteString'.

>>> one 97 :: LByteString
"a"

prop> LByteString.length (one x) == 1
-}
instance One BSL.ByteString where
    type OneItem BSL.ByteString = Word8
    one = BSL.singleton
    {-# INLINE one #-}

-- Maps

{- | Create singleton 'Map' from key-value pair.

>>> one (3, "foo") :: Map Int Text
fromList [(3,"foo")]

prop> length (one @(Map Int String) x) == 1
-}
instance One (Map k v) where
    type OneItem (Map k v) = (k, v)
    one = uncurry M.singleton
    {-# INLINE one #-}

{- | Create singleton 'HashMap' from key-value pair.

>>> one (3, "foo") :: HashMap Int Text
fromList [(3,"foo")]

prop> length (one @(HashMap Int String) x) == 1
-}
instance Hashable k => One (HashMap k v) where
    type OneItem (HashMap k v) = (k, v)
    one = uncurry HM.singleton
    {-# INLINE one #-}

{- | Create singleton 'IntMap' from key-value pair.

>>> one (3, "foo") :: IntMap Text
fromList [(3,"foo")]

prop> length (one @(IntMap String) x) == 1
-}
instance One (IntMap v) where
    type OneItem (IntMap v) = (Int, v)
    one = uncurry IM.singleton
    {-# INLINE one #-}

-- Sets

{- | Create singleton 'Set'.

>>> one 42 :: Set Int
fromList [42]

prop> length (one @(Set Int) x) == 1
-}
instance One (Set v) where
    type OneItem (Set v) = v
    one = Set.singleton
    {-# INLINE one #-}

{- | Create singleton 'HashSet'.

>>> one 42 :: HashSet Int
fromList [42]

prop> length (one @(HashSet Int) x) == 1
-}
instance Hashable v => One (HashSet v) where
    type OneItem (HashSet v) = v
    one = HashSet.singleton
    {-# INLINE one #-}

{- | Create singleton 'IntSet'.

>>> one 42 :: IntSet
fromList [42]

prop> IntSet.size (one x) == 1
-}
instance One IntSet where
    type OneItem IntSet = Int
    one = IS.singleton
    {-# INLINE one #-}
