{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Lifted
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Lifted versions of base functions.

These functions are lifted in a sense that you can use them inside various
monad transformers without adding 'Relude.Monad.Reexport.liftIO' calls
explicitly. However, you still can use all these functions inside plain
 t'Relude.Base.IO' monad as usual.

=== Example

==== @base@

With the @base@ function, you can easily work with these functions in the
 t'Relude.Base.IO' monad:

@
__main__ :: t'Relude.Base.IO' ()
__main__ = __do__
    x <- 'Data.Text.getLine'
    'Prelude.print' x
@

However, to work in t'Relude.Monad.Reexport.MonadIO' you already need to "lift" them:

@
__main__ :: t'Relude.Monad.Reexport.MonadIO' m => m ()
__main__ = __do__
    x <- 'Relude.Monad.Reexport.liftIO' 'Data.Text.getLine'
    'Relude.Monad.Reexport.liftIO' ('Prelude.print' x)
@

==== @relude@

In the meantime, @relude@ provides these function that can work in t'Relude.Base.IO'
the same way:

@
__main__ :: t'Relude.Base.IO' ()
__main__ = __do__
    x <- 'getLine'
    'print' x
@

But also allows you to work in the t'Relude.Monad.Reexport.MonadIO' monads more easily:

@
__main__ :: t'Relude.Monad.Reexport.MonadIO' m => m ()
__main__ = __do__
    x <- 'getLine'
    'print' x
@
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
    , module Relude.Lifted.Handle
      -- $handle
    , module Relude.Lifted.Env
      -- $env
    ) where

import Relude.Lifted.Concurrent
import Relude.Lifted.Exit
import Relude.Lifted.File
import Relude.Lifted.IORef
import Relude.Lifted.Terminal
import Relude.Lifted.Handle
import Relude.Lifted.Env

{- $concurrent
Lifted 'MVar' and 'STM' functions.
-}
{- $exit
Lifted versions of functions that work with exit processes.
-}
{- $file
Lifted versions of functions working with files and common t'Relude.Base.IO'.
-}
{- $ioref
Lifted reexports from "Data.IORef" module.
-}
{- $terminal
Lifted functions to work with stdin and stdout.
-}
{- $handle
Lifted functions to work with t'Relude.Base.IO' 'Handle's.
-}
{- $env
Lifted functions to work with system environment.
-}
