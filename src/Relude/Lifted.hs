{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lifted versions of base functions.

These functions are lifted in a sense that you can use them inside various
monad transformers without adding 'Relude.liftIO' calls explicitly. However, you still
can use all these functions inside plain 'Relude.IO' monad as usual.
-}

module Relude.Lifted
    ( module Relude.Lifted.Concurrent
      -- $concurrent
    , module Relude.Lifted.IORef
      -- $ioref
    , module Relude.Lifted.Exit
      -- $exit
    , module Relude.Lifted.File
      -- $file
    , module Relude.Lifted.Terminal
      -- $terminal
    ) where

import Relude.Lifted.Concurrent
import Relude.Lifted.Exit
import Relude.Lifted.File
import Relude.Lifted.IORef
import Relude.Lifted.Terminal

{- $concurrent
Lifted 'MVar' and 'STM' functions.
-}
{- $exit
Lifted versions of functions that work with exit processes.
-}
{- $file
Lifted versions of functions working with files and common 'Relude.IO'.
-}
{- $ioref
Lifted reexports from "Data.IORef" module.
-}
{- $terminal
Lifted functions to work with stdin and stdout.
-}
