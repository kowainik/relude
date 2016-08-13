{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Debug (
  undefined,
  error,
  trace,
  traceM,
  traceId,
  traceIO,
  traceShow,
  traceShowId,
  traceShowM,
  notImplemented,
) where

import Data.Text (Text, unpack)
import Control.Monad (Monad, return)

import qualified Base as P
import Show (Print, putStrLn)

import System.IO.Unsafe (unsafePerformIO)

{-# WARNING trace "'trace' remains in code" #-}
trace :: Print b => b -> a -> a
trace string expr = unsafePerformIO (do
    putStrLn string
    return expr)

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: Print b => b -> a -> P.IO a
traceIO string expr = do
    putStrLn string
    return expr

{-# WARNING error "'error' remains in code" #-}
error :: Text -> a
error s = P.error (unpack s)

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> b -> b
traceShow a b = trace (P.show a) b

{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = trace (P.show a) a

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, Monad m) => a -> m ()
traceShowM a = trace (P.show a) (return ())

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => Text -> m ()
traceM s = trace (unpack s) (return ())

{-# WARNING traceId "'traceM' remains in code" #-}
traceId :: Text -> Text
traceId s = trace s s

notImplemented :: a
notImplemented = P.error "Not implemented"

undefined :: a
undefined = P.undefined
