{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Bool
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Convenient commonly used and very helpful functions to work with 'Bool' and also
with monads.
-}

module Relude.Bool
    ( module Relude.Bool.Reexport
      -- $reexport
    , module Relude.Bool.Extra
      -- $reexport
    , module Relude.Bool.Guard
      -- $guard
    ) where

import Relude.Bool.Extra
import Relude.Bool.Guard
import Relude.Bool.Reexport

{- $reexport
Reexports from "Data.Bool" and "Control.Monad". Includes necessary types and
functions to work with 'Bool' type.
-}

{- $guard
Boolean combinators that work in monads. Like 'ifM' or 'guardM'.
-}
