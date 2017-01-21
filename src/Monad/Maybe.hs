-- | Utility functions to work with 'Data.Maybe' data type as monad.

module Monad.Maybe
       ( whenJust
       , whenJustM
       ) where

import           Control.Applicative (Applicative, pure)
import           Control.Monad       (Monad (..), (=<<))
import           Data.Maybe          (Maybe (..), maybe)

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pure ()
{-# INLINE whenJust #-}

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mm f = maybe (return ()) f =<< mm
{-# INLINE whenJustM #-}
