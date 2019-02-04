{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports "Data.Foldable" and "Data.Traversable".
-}

module Relude.Foldable.Reexport
       ( module Data.Foldable
       , module Data.Traversable
#if MIN_VERSION_base(4,10,0)
       , module Data.Bifoldable
       , module Data.Bitraversable
#endif
       ) where

import Data.Foldable (Foldable (fold, foldMap, foldl', foldr, length, null, toList), all, and, any,
                      asum, concat, concatMap, find, foldlM, forM_, for_, mapM_, or, sequenceA_,
                      sequence_, traverse_)
import Data.Traversable (Traversable (..), forM, mapAccumL, mapAccumR)
#if MIN_VERSION_base(4,10,0)
import Data.Bifoldable (Bifoldable (..), biList, biall, biand, biany, biasum, bielem, bifind,
                        bifoldl', bifoldlM, bifoldr', bifoldrM, bifor_, bilength, binull, bior,
                        bisequence_, bitraverse_)
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bifor, bimapDefault, bisequence)
#endif
