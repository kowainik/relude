{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018 astynax
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module reexports some 'Bitraversable' related stuff.
-}

module Relude.Bitraversable
       (
#if MIN_VERSION_base(4,10,0)
         Bitraversable (..)
       , bisequence
       , bifor
       , bimapDefault
       , bifoldMapDefault
#endif
       ) where

#if MIN_VERSION_base(4,10,0)
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bifor, bimapDefault, bisequence)
#endif
