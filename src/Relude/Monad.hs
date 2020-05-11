{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Reexporting useful monadic stuff.
-}

module Relude.Monad
    ( module Relude.Monad.Reexport
      -- $reexport
    , module Relude.Monad.Maybe
      -- $maybe
    , module Relude.Monad.Either
      -- $either
    , module Relude.Monad.Trans
      -- $trans
    , chainedTo
    ) where

import Relude.Monad.Either
import Relude.Monad.Maybe
import Relude.Monad.Reexport
import Relude.Monad.Trans


-- $setup
-- >>> import Relude

{- | For chaining monadic operations in forward applications using '(&)'
Named version of '=<<'.

>>> Just [ 1 :: Int ] & chainedTo (viaNonEmpty head)
Just 1
>>> Nothing & chainedTo (viaNonEmpty head)
Nothing
-}
chainedTo :: Monad m => (a -> m b) -> m a -> m b
chainedTo = (=<<)
{-# INLINE chainedTo #-}

{- $reexport
Reexports functions to work with different monads.
-}
{- $maybe
Provided new combinators to work with 'Relude.Maybe' data type.
-}
{- $either
Provided new combinators to work with 'Relude.Either' data type.
-}
{- $trans
Monad transformers functions and combinators.
-}
