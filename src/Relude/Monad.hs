{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexporting useful monadic stuff.
-}

module Relude.Monad
       ( module Relude.Monad.Either
       , module Relude.Monad.Maybe
       , module Relude.Monad.Reexport
       , module Relude.Monad.Trans
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
