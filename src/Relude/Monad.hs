{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2021 Kowainik
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
    , infinitely
    ) where

import Relude.Monad.Either
import Relude.Monad.Maybe
import Relude.Monad.Reexport
import Relude.Monad.Trans
import Control.Applicative (Applicative)
import Relude.Base (Void)


-- $setup
-- >>> import Relude

{- | For chaining monadic operations in forward applications using '(&)'
Named version of '=<<'.

>>> Just [ 1 :: Int ] & chainedTo (viaNonEmpty head)
Just 1
>>> Nothing & chainedTo (viaNonEmpty head)
Nothing

@since 0.5.0
-}
chainedTo :: Monad m => (a -> m b) -> m a -> m b
chainedTo = (=<<)
{-# INLINE chainedTo #-}

{- | Repeat a monadic action indefinitely.

This is a more type safe version of 'forever', which has a convinient
but unsafe type.

Consider the following two examples. In the @getIntForever@ functions, it
falsely expects 'Int' as the result of the 'forever' function. But it would need
to wait *forever* to get that, and this mistake won't be caught by the type
system and compiler:

@
getIntForever :: IO Int
getIntForever = do
    i <- forever $ do ...
    pure i
@

In contrast, using 'infinitely' instead of 'forever' in 'foo' is a type error.

@since 1.0.0.0
-}
infinitely :: Applicative f => f a -> f Void
infinitely = forever
{-# INLINE infinitely #-}

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
