module Panic (
  FatalError(..),
  panic,
) where

import Base (Show)
import Data.Text(Text)
import Control.Exception as X

-- | Uncatchable exceptions thrown and never caught.
data FatalError = FatalError { msg :: Text }
  deriving (Show)

instance Exception FatalError

panic :: Text -> a
panic a = throw (FatalError a)
