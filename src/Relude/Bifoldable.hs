{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018 astynax
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module reexports some 'Bifoldable' related stuff.
-}

module Relude.Bifoldable
       (
#if MIN_VERSION_base(4,10,0)
         Bifoldable (..)
       , bifoldl'
       , bifoldl1
       , bifoldlM
       , bifoldr'
       , bifoldr1
       , bifoldrM
       , bitraverse_
       , bifor_
       , biasum
       , bisequence_
       , biList
       , binull
       , bilength
       , bielem
       , biand
       , bior
       , biany
       , biall
       , bifind
#endif
       ) where

#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable (Bifoldable (..), biList, biall, biand,
                        biany, biasum, bielem, bifind, bifoldl', bifoldl1, bifoldlM, bifoldr',
                        bifoldr1, bifoldrM, bifor_, bilength, binull, bior, bisequence_,
                        bitraverse_)
#endif
