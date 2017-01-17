{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy        #-}

module Panic
       ( FatalError (..)
       , panic
       ) where

import           Base              (Show)
import           Control.Exception as X
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)

-- | Uncatchable exceptions thrown and never caught.
data FatalError = FatalError { fatalErrorMessage :: Text }
  deriving (Show, Typeable)

instance Exception FatalError

panic :: Text -> a
panic a = throw (FatalError a)
