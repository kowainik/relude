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
       ( type AllOf
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
-- f :: AllOf [Show, Read] [a, b] => a -> b -> String
-- =
-- f :: (Show a, Show b, Read a, Show b) => a -> b -> String
-- @
--
-- To specify list with single constraint / variable, don't forget to prefix
-- it with @\'@:
--
-- @
-- f :: AllOf '[Show] [a, b] => a -> b -> String
-- @
type family AllOf (c :: [k -> Constraint]) (as :: [k]) where
    AllOf c '[] = (() :: Constraint)
    AllOf c (h ': t) = (c <+> h, AllOf c t)
