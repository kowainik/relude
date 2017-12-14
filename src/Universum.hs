{-# LANGUAGE Trustworthy #-}

-- | Main module that reexports all functionality allowed to use
-- without importing any other modules. Just add next lines to your
-- module to replace default ugly 'Prelude' with better one.
--
-- @
--     {-\# LANGUAGE NoImplicitPrelude \#-}
--
--     import Universum
-- @

module Universum
       ( -- * Reexports from base and from modules in this repo
         module Universum.Applicative
       , module Universum.Base
       , module Universum.Bool
       , module Universum.Container
       , module Universum.Debug
       , module Universum.DeepSeq
       , module Universum.Exception
       , module Universum.Function
       , module Universum.Functor
       , module Universum.Lifted
       , module Universum.List
       , module Universum.Monad
       , module Universum.Monoid
       , module Universum.Nub
       , module Universum.Print
       , module Universum.String
       , module Universum.TypeOps
       , module Universum.VarArg

         -- * Safe functions
       , module Safe

         -- * Lenses
       , module Lens.Micro
       , module Lens.Micro.Mtl
       ) where

import Universum.Applicative
import Universum.Bool
import Universum.Container
import Universum.Debug
import Universum.DeepSeq
import Universum.Exception
import Universum.Function
import Universum.Functor
import Universum.Lifted
import Universum.List
import Universum.Monad
import Universum.Monoid
import Universum.Nub
import Universum.Print
import Universum.String
import Universum.TypeOps
import Universum.VarArg

import Universum.Base hiding (error, show, showFloat, showList, showSigned, showSignedFloat,
                       showsPrec, undefined)

-- Maybe'ized version of partial functions
import Safe (atDef, atMay, foldl1May, foldr1May, headDef, headMay, initDef, initMay, initSafe,
             lastDef, lastMay, tailDef, tailMay, tailSafe)

-- Lenses
import Lens.Micro (Lens, Lens', Traversal, Traversal', over, set, (%~), (&), (.~), (<&>), (^.),
                   (^..), (^?), _1, _2, _3, _4, _5)
import Lens.Micro.Mtl (preuse, preview, use, view)
