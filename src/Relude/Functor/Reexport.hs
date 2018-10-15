{-# LANGUAGE CPP    #-}
{-# LANGUAGE Safe #-}

{- |
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports functionality regarding 'Functor' and 'Bifunctor' typeclasses.
-}

module Relude.Functor.Reexport
       ( module Data.Bifunctor
       , module Data.Functor
       , module Data.Functor.Compose
       , module Data.Functor.Identity
#if MIN_VERSION_base(4,12,0)
        , module Data.Functor.Contravariant
 #endif
       ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor (Functor (..), void, ($>), (<$>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif
