{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Panic (
  FatalError(..),
  panic,
) where

import Base (Show)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Control.Exception as X

-- | Uncatchable exceptions thrown and never caught.
data FatalError = FatalError { fatalErrorMessage :: Text }
  deriving (Show, Typeable)

instance Exception FatalError

panic :: Text -> a
panic a = throw (FatalError a)
