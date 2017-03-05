{-# LANGUAGE CPP                #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module TypeOps
       ( type Each
       , type ($)
       ) where

#if __GLASGOW_HASKELL__ <= 710
import           GHC.Prim              (Constraint)
#else
import           Data.Kind             (Constraint)
#endif

import           Control.Type.Operator (type ($), type (<+>))

-- | Map several constraints over a several variables.
--
-- @
-- f :: Each [Show, Read] [a, b] => a -> b -> String
-- =
-- f :: (Show a, Show b, Read a, Show b) => a -> b -> String
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
