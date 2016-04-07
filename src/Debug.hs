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

--{-# WARNING undefined "Do not use 'undefined' in production code" #-}
undefined :: a
undefined = P.undefined

--{-# WARNING error "Do not use 'error' in production code" #-}
error :: P.String -> a
error = P.error

--{-# WARNING trace "Do not use 'trace' in production code" #-}
trace :: P.String -> a -> a
trace = T.trace

--{-# WARNING trace "Do not use 'trace' in production code" #-}
traceShow :: P.Show a => a -> a
traceShow a = T.trace (P.show a) a

--{-# WARNING trace "Do not use 'trace' in production code" #-}
traceShowM :: (P.Show a, P.Monad m) => a -> m ()
traceShowM a = T.traceM (P.show a)

--{-# WARNING traceM "Do not use 'traceM' in production code" #-}
traceM :: P.Monad m => P.String -> m ()
traceM = T.traceM

--{-# WARNING traceIO "Do not use 'traceIO' in production code" #-}
traceIO :: P.String -> P.IO ()
traceIO = T.traceIO

notImplemented :: a
notImplemented = P.error "Not implemented"
