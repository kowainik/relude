{-# LANGUAGE Safe #-}

{- |
Module                  : Relude.Monad.Reexport
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Reexports functions to work with monads.
-}

module Relude.Monad.Reexport
    ( -- * Reexport transformers
      module Control.Monad.Except
    , module Control.Monad.Reader
    , module Control.Monad.State.Strict
    , module Control.Monad.Trans
    , module Control.Monad.Trans.Identity
    , module Control.Monad.Trans.Maybe

      -- * Reexport monadic functions
    , module Control.Monad
    , module Control.Monad.Fail

      -- * Reexport 'Maybe'
    , module Data.Maybe

      -- * Reexport 'Either'
    , module Data.Either
    ) where

-- Monad transformers
import Control.Monad.Except (MonadError, ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, Reader, ReaderT (..), ask, asks, local, reader, runReader,
                             withReader, withReaderT)
import Control.Monad.State.Strict (MonadState, State, StateT (..), evalState, evalStateT, execState,
                                   execStateT, get, gets, modify, modify', put, runState, state,
                                   withState)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Trans.Identity (IdentityT (runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT (..), exceptToMaybeT, maybeToExceptT)

-- Control.Monad
import Control.Monad (Monad (return, (>>), (>>=)), MonadPlus (..), filterM, forever, join,
                      mapAndUnzipM, mfilter, replicateM, replicateM_, zipWithM, zipWithM_, (<$!>),
                      (<=<), (=<<), (>=>))
import Control.Monad.Fail (MonadFail (..))

-- Maybe
import Data.Maybe (Maybe (..), catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe,
                   maybe, maybeToList)

-- Either
import Data.Either (Either (..), either, isLeft, isRight, lefts, partitionEithers, rights)
