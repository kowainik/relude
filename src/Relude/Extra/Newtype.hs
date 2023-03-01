{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Extra.Newtype
Copyright               : (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Functions to ease work with @newtypes@.

@since 0.2.0
-}

module Relude.Extra.Newtype
    ( un
    , wrap
    , under
    , under2
    , underF2
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

@since 0.2.0
-}
un :: forall a n . Coercible a n => n -> a
un = coerce
{-# INLINE un #-}

{- | Wraps value to @newtype@. Behaves exactly as 'un' but has more meaningful
name in case you need to convert some value to @newtype@.

>>> newtype Flag = Flag Bool deriving (Show, Eq)
>>> wrap False == Flag True
False

@since 0.2.0
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

@since 0.2.0
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

@since 0.3.0
-}
under2 :: forall n a . Coercible a n => (n -> n -> n) -> (a -> a -> a)
under2 = coerce
{-# INLINE under2 #-}

{- | Version of 'under2' that works on @newtype@s parametrized by their
representation. Provided for convenience.

>>> underF2 @Sum (<>) (3 :: Int) 4
7
>>> underF2 @Max (<>) 'p' 't'
't'

@since 0.3.0
-}
underF2 :: forall n a . Coercible a (n a) => (n a -> n a -> n a) -> (a -> a -> a)
underF2 = coerce
{-# INLINE underF2 #-}

{- | Coercible composition. This function allows to write more efficient
implementations of function compositions over @newtypes@.

@since 0.3.0
-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
