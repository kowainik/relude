{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

This module reexports very basic and primitive functions and function
combinators.
-}

module Relude.Function
    ( -- * Reexports
      -- ** "Control.Arrow" reexports
      (&&&)
      -- ** "Control.Category" reexports
    , (>>>)
    , (<<<)
      -- ** "Data.Function" reexports
    , (.)
    , ($)
    , (&)
    , id
    , const
    , flip
    , fix
    , on

      -- * Combinators
    , identity
    ) where

import Control.Arrow ((&&&))
import Control.Category ((<<<), (>>>))
import Data.Function (const, fix, flip, id, on, ($), (&), (.))


-- $setup
-- >>> import Relude

{- | Renamed version of 'Prelude.id'.

>>> identity 10
10

>>> fmap identity [1,2,3]
[1,2,3]

-}
identity :: a -> a
identity = id
{-# INLINE identity #-}
