{-# LANGUAGE ScopedTypeVariables #-}

{- | Functions to ease work with newtypes.
-}

module Relude.Extra.Newtype
       ( un
       , wrap
       , under
       ) where

import Relude

-- $setup
-- >>> :set -XTypeApplications

{- | Unwraps value from @newtype@.

>>> newtype Size = Size Int deriving Show
>>> un @Int (Size 5)
5
>>> un (Size 5) == length ['a', 'x', 'b']
False
-}
un :: forall b a . Coercible a b => a -> b
un = coerce

{- | Wraps value to @newtype@. Behaves exactly as 'un' but has more meaningnful
name in case you need to convert some value to @newtype@.

>>> newtype Flag = Flag Bool deriving (Show, Eq)
>>> wrap False == Flag True
False
-}
wrap :: forall b a . Coercible a b => a -> b
wrap = coerce

{- | Applies function to the content of @newtype@. This function is not supposed
to be used on @newtype@s that are created with the help of smart constructors.

>>> newtype Foo = Foo Bool deriving Show
>>> under not (Foo True)
Foo False
>>> newtype Bar = Bar String deriving Show
>>> under (filter (== 'a')) (Bar "abacaba")
Bar "aaaa"
-}
under :: forall b a . Coercible a b => (b -> b) -> a -> a
under = coerce
