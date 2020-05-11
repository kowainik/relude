{-# LANGUAGE ExplicitForAll #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted functions to work with stdin and stdout.
-}

module Relude.Lifted.Terminal
    ( getLine
    , print
    , putStr
    , putStrLn
    ) where

import Relude.Base (IO, Show)
import Relude.Function ((.))
import Relude.Monad.Reexport (MonadIO (..))
import Relude.String.Reexport (String, Text)

import qualified Data.Text.IO as TIO
import qualified System.IO as IO (print, putStr, putStrLn)


-- | Lifted version of 'Data.Text.getLine'.
getLine :: MonadIO m => m Text
getLine = liftIO TIO.getLine
{-# SPECIALIZE getLine :: IO Text #-}
{-# INLINE getLine #-}

-- | Lifted version of 'Prelude.print'.
print :: forall a m . (MonadIO m, Show a) => a -> m ()
print = liftIO . IO.print
{-# SPECIALIZE print :: Show a => a -> IO () #-}
{-# INLINE print #-}

-- | Lifted version of 'IO.putStr'.
putStr :: MonadIO m => String -> m ()
putStr = liftIO . IO.putStr
{-# SPECIALIZE putStr :: String -> IO () #-}
{-# INLINE putStr #-}

-- | Lifted version of 'IO.putStrLn'.
putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . IO.putStrLn
{-# SPECIALIZE putStrLn :: String -> IO () #-}
{-# INLINE putStrLn #-}
