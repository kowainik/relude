{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE TypeFamilies      #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

'One' is a typeclass for creating structures from a singleton element.
It has three main goals:

1. Give a shorter name for the construction: uses 'one' instead of common @singleton@.
2. Work with monomorphic structures like 'T.Text' or 'IntSet'.
3. Give a clearer and less scary name for cases where you can use 'Relude.pure' or @(:[])@.

@since 0.1.0
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
import qualified Data.ByteString.Short as SBS

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

{- | Typeclass for data types that can be created from one element.
E.g. lists, non-empty containers, maps.

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
    -- | Type of a single element of the structure.
    type OneItem x

    -- | Create a list, map, 'T.Text', etc from a single element.
    one :: OneItem x -> x

-- Lists

{- | Allows to create a singleton list. You might prefer function with name 'one'
instead of 'Relude.pure' or @(:[])@.

>>> one 42 :: [Int]
[42]

@
law> 'Relude.length' ('one' @[a] x) ≡ 1
@
-}
instance One [a] where
    type OneItem [a] = a

    one :: a -> [a]
    one = (:[])
    {-# INLINE one #-}

{- | Allows to create singleton 'NE.NonEmpty' list. You might prefer function with
name 'one' instead of 'Relude.pure' or @(:|[])@.

>>> one 42 :: NonEmpty Int
42 :| []

@
law> 'Relude.length' ('one' @('NE.NonEmpty' a) x) ≡ 1
@
-}
instance One (NE.NonEmpty a) where
    type OneItem (NE.NonEmpty a) = a

    one :: a -> NE.NonEmpty a
    one = (NE.:|[])
    {-# INLINE one #-}

{- | Create singleton 'SEQ.Seq'.

>>> one 42 :: Seq Int
fromList [42]

@
law> 'Relude.length' ('one' @('SEQ.Seq' a) x) ≡ 1
@
-}
instance One (SEQ.Seq a) where
    type OneItem (SEQ.Seq a) = a

    one :: a -> SEQ.Seq a
    one = SEQ.singleton
    {-# INLINE one #-}

-- Monomorphic sequences

{- | Create singleton strict 'T.Text'.

>>> one 'a' :: Text
"a"

@
law> 'Data.Text.length' ('one' x) ≡ 1
@
-}
instance One T.Text where
    type OneItem T.Text = Char

    one :: Char -> T.Text
    one = T.singleton
    {-# INLINE one #-}

{- | Create singleton lazy 'TL.Text'.

>>> one 'a' :: LText
"a"

@
law> 'Data.Text.Lazy.length' ('one' x) ≡ 1
@
-}
instance One TL.Text where
    type OneItem TL.Text = Char

    one :: Char -> TL.Text
    one = TL.singleton
    {-# INLINE one #-}

{- | Create singleton strict 'BS.ByteString'.

>>> one 97 :: ByteString
"a"

@
law> 'BS.length' ('one' x) ≡ 1
@
-}
instance One BS.ByteString where
    type OneItem BS.ByteString = Word8

    one :: Word8 -> BS.ByteString
    one = BS.singleton
    {-# INLINE one #-}

{- | Create singleton lazy 'BSL.ByteString'.

>>> one 97 :: LByteString
"a"

@
law> 'BSL.length' ('one' x) ≡ 1
@
-}
instance One BSL.ByteString where
    type OneItem BSL.ByteString = Word8

    one :: Word8 -> BSL.ByteString
    one = BSL.singleton
    {-# INLINE one #-}

{- | Create singleton 'SBS.ShortByteString'.

>>> one 97 :: ShortByteString
"a"

@
law> 'SBS.length' ('one' x) ≡ 1
@
-}
instance One SBS.ShortByteString where
    type OneItem SBS.ShortByteString = Word8

    one :: Word8 -> SBS.ShortByteString
    one x = SBS.pack [x]
    {-# INLINE one #-}

-- Maps

{- | Create singleton 'Map' from key-value pair.

>>> one (3, "foo") :: Map Int Text
fromList [(3,"foo")]

@
law> 'Relude.length' ('one' @('Map' k v) (k, v)) ≡ 1
@
-}
instance One (Map k v) where
    type OneItem (Map k v) = (k, v)

    one :: (k, v) -> Map k v
    one = uncurry M.singleton
    {-# INLINE one #-}

{- | Create singleton 'HashMap' from key-value pair.

>>> one (3, "foo") :: HashMap Int Text
fromList [(3,"foo")]

@
law> 'Relude.length' ('one' @('HashMap' k v) (k, v)) ≡ 1
@
-}
instance Hashable k => One (HashMap k v) where
    type OneItem (HashMap k v) = (k, v)

    one :: (k, v) -> HashMap k v
    one = uncurry HM.singleton
    {-# INLINE one #-}

{- | Create singleton 'IntMap' from key-value pair.

>>> one (3, "foo") :: IntMap Text
fromList [(3,"foo")]

@
law> 'Relude.length' ('one' @('IntMap' a) x) ≡ 1
@
-}
instance One (IntMap v) where
    type OneItem (IntMap v) = (Int, v)

    one :: (Int, v) -> IntMap v
    one = uncurry IM.singleton
    {-# INLINE one #-}

-- Sets

{- | Create singleton 'Set'.

>>> one 42 :: Set Int
fromList [42]

@
law> 'Relude.length' ('one' @('Set' a) x) ≡ 1
@
-}
instance One (Set a) where
    type OneItem (Set a) = a

    one :: a -> Set a
    one = Set.singleton
    {-# INLINE one #-}

{- | Create singleton 'HashSet'.

>>> one 42 :: HashSet Int
fromList [42]

@
law> 'Relude.length' ('one' @('HashSet' a) x) ≡ 1
@
-}
instance Hashable a => One (HashSet a) where
    type OneItem (HashSet a) = a

    one :: a -> HashSet a
    one = HashSet.singleton
    {-# INLINE one #-}

{- | Create singleton 'IntSet'.

>>> one 42 :: IntSet
fromList [42]

@
law> 'Data.IntSet.size' ('one' x) ≡ 1
@
-}
instance One IntSet where
    type OneItem IntSet = Int

    one :: Int -> IntSet
    one = IS.singleton
    {-# INLINE one #-}
