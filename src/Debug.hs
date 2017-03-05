{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE Trustworthy        #-}

module Debug
       ( undefined
       , error
       , trace
       , traceM
       , traceId
       , traceIO
       , traceShow
       , traceShowId
       , traceShowM
       , notImplemented
       , NotImplemented(..)
       ) where

import           Control.Monad          (Monad, return)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Data              (Data)
import           Data.Text              (Text, unpack)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import           System.IO.Unsafe       (unsafePerformIO)

import qualified Base                   as P
import qualified Prelude                as P
import           Show                   (Print, putStrLn)

import           Applicative            (pass)

{-# WARNING trace "'trace' remains in code" #-}
trace :: Print b => b -> a -> a
trace string expr = unsafePerformIO (do
    putStrLn string
    return expr)

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: (Print b, MonadIO m) => b -> a -> m a
traceIO string expr = do
    putStrLn string
    return expr

{-# WARNING error "'error' remains in code (or use 'panic')" #-}
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
traceShowM a = trace (P.show a) pass

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => Text -> m ()
traceM s = trace (unpack s) pass

{-# WARNING traceId "'traceM' remains in code" #-}
traceId :: Text -> Text
traceId s = trace s s

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = P.error "Not implemented"

{-# WARNING NotImplemented "'NotImplemented' remains in code" #-}
data NotImplemented = NotImplemented
    deriving (P.Eq, P.Ord, P.Show, Data, Typeable, Generic)

{-# WARNING undefined "'undefined' remains in code (or use 'panic')" #-}
undefined :: a
undefined = P.undefined
