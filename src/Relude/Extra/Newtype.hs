{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to ease work with newtypes.
-}

module Relude.Extra.Newtype
       ( un
       , wrap
       , under
       , under2
#if ( __GLASGOW_HASKELL__ != 802 )
       , underF2
#endif
       , (#.)
       ) where

import Relude


-- $setup
-- >>> :set -XTypeApplications
-- >>> import Data.Semigroup (Max (..))

{- | Unwraps value from @newtype@.

>>> newtype Size = Size Int deriving Show
>>> un @Int (Size 5)
5
>>> un (Size 5) == length ['a', 'x', 'b']
False
-}
un :: forall a n . Coercible a n => n -> a
un = coerce
{-# INLINE un #-}

{- | Wraps value to @newtype@. Behaves exactly as 'un' but has more meaningnful
name in case you need to convert some value to @newtype@.

>>> newtype Flag = Flag Bool deriving (Show, Eq)
>>> wrap False == Flag True
False
-}
wrap :: forall n a . Coercible a n => a -> n
wrap = coerce
{-# INLINE wrap #-}

{- | Applies function to the content of @newtype@. This function is not supposed
to be used on @newtype@s that are created with the help of smart constructors.

>>> newtype Foo = Foo Bool deriving Show
>>> under not (Foo True)
Foo False
>>> newtype Bar = Bar String deriving Show
>>> under (filter (== 'a')) (Bar "abacaba")
Bar "aaaa"
-}
under :: forall n a . Coercible a n => (n -> n) -> (a -> a)
under = coerce
{-# INLINE under #-}

{- | Lift binary function for @newtype@s to work over underlying @newtype@
representation.

>>> under2 @(Sum Int) (<>) (3 :: Int) 4
7
>>> under2 @All (<>) True False
False
-}
under2 :: forall n a . Coercible a n => (n -> n -> n) -> (a -> a -> a)
under2 = coerce
{-# INLINE under2 #-}

#if ( __GLASGOW_HASKELL__ != 802 )
{- | Version of 'under2' that works on @newtype@s parametrized by their
representation. Provided for convenience.

>>> underF2 @Sum (<>) (3 :: Int) 4
7
>>> underF2 @Max (<>) 'p' 't'
't'
-}
underF2 :: forall n a . Coercible a (n a) => (n a -> n a -> n a) -> (a -> a -> a)
underF2 = coerce
{-# INLINE underF2 #-}
#endif

{- | Coercible composition. This function allows to write more efficient
implementations of functions compoitions over @newtypes@.
-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
