{-# LANGUAGE CPP                #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

#if __GLASGOW_HASKELL__ <= 710
{-# LANGUAGE Trustworthy        #-}
#else
{-# LANGUAGE Safe               #-}
#endif

-- | Type operators for writing convenient type signatures.

module TypeOps
       ( type Each
       , type With
       , type ($)
       ) where

#if __GLASGOW_HASKELL__ <= 710
import           GHC.Prim              (Constraint)
#else
import           Data.Kind             (Constraint)
#endif

import           Control.Type.Operator (type ($), type (<+>))

-- | Map several constraints over several variables.
--
-- @
-- f :: Each [Show, Read] [a, b] => a -> b -> String
-- =
-- f :: (Show a, Show b, Read a, Read b) => a -> b -> String
-- @
--
-- To specify list with single constraint / variable, don't forget to prefix
-- it with @\'@:
--
-- @
-- f :: Each '[Show] [a, b] => a -> b -> String
-- @
type family Each (c :: [k -> Constraint]) (as :: [k]) where
    Each c '[] = (() :: Constraint)
    Each c (h ': t) = (c <+> h, Each c t)

-- | Map several constraints over a single variable.
-- Note, that @With a b ≡ Each a '[b]@
--
-- @
-- a :: With [Show, Read] a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
-- @
type With a b = a <+> b
