{-# LANGUAGE NoImplicitPrelude #-}

module Debug (
    undefined
  , error
  , trace
  , traceM
  , traceIO
  , traceShow
  , traceShowM
  , notImplemented
  ) where

import qualified Prelude as P
import qualified Debug.Trace as T

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = P.undefined

{-# WARNING error "'error' remains in code" #-}
error :: P.String -> a
error = P.error

{-# WARNING trace "'trace' remains in code" #-}
trace :: P.String -> a -> a
trace = T.trace

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> a
traceShow a = T.trace (P.show a) a

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, P.Monad m) => a -> m ()
traceShowM a = T.traceM (P.show a)

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: P.Monad m => P.String -> m ()
traceM = T.traceM

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: P.String -> P.IO ()
traceIO = T.traceIO

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = P.error "Not implemented"
