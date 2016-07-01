{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Debug (
  undefined,
  error,
  trace,
  traceM,
  traceIO,
  traceShow,
  traceShowM,
  notImplemented,
) where

import Data.Text (Text, unpack)
import Control.Monad (Monad, return)

import qualified Base as P
import qualified Debug.Trace as T

{-# WARNING error "'error' remains in code" #-}
error :: Text -> a
error s = P.error (unpack s)

{-# WARNING trace "'trace' remains in code" #-}
trace :: Text -> a -> a
trace s = T.trace (unpack s)

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> b -> b
traceShow a b = T.trace (P.show a) b

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, Monad m) => a -> m ()
traceShowM a = T.trace (P.show a) (return ())

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => Text -> m ()
traceM s = T.trace (unpack s) (return ())

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: Text -> P.IO ()
traceIO s = T.traceIO (unpack s)

notImplemented :: a
notImplemented = P.error "Not implemented"

undefined :: a
undefined = P.undefined
