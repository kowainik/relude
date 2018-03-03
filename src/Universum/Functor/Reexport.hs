{-# LANGUAGE Safe #-}

-- | This module reexports functionality regarding 'Functor' type class.

module Universum.Functor.Reexport
       ( module Control.Arrow
       , module Data.Bifunctor
       , module Data.Functor
       , module Data.Functor.Compose
       , module Data.Functor.Identity
       ) where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (Functor (..), void, ($>), (<$>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
