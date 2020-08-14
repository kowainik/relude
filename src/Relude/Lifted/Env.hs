{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lifted functions to work with system enviroment.

@since 0.8.0.0
-}

module Relude.Lifted.Env
    ( getArgs
    , lookupEnv
    ) where

import Relude.Base (IO)
import Relude.Function ((.))
import Relude.Monad.Reexport (MonadIO (..), Maybe)
import Relude.String.Reexport (String)

import qualified System.Environment as ENV (getArgs, lookupEnv)

{- | Lifted version of 'System.Environment.getArgs'.

@since 0.8.0.0
-}
getArgs :: MonadIO m => m [String]
getArgs = liftIO ENV.getArgs
{-# SPECIALIZE getArgs :: IO [String] #-}
{-# INLINE getArgs #-}

{- | Lifted version of 'System.Environment.lookupEnv'.

@since 0.8.0.0
-}
lookupEnv :: MonadIO m => String -> m (Maybe String)
lookupEnv = liftIO . ENV.lookupEnv
{-# SPECIALIZE lookupEnv :: String -> IO (Maybe String) #-}
{-# INLINE lookupEnv #-}
