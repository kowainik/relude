{-# LANGUAGE Safe #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module reexports functionality regarding 'Functor' type class.

module Relude.Functor.Reexport
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
