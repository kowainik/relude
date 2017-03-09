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
       , traceShow
       , traceShowId
       , traceShowM
       , Undefined (..)
       ) where

import           Control.Monad    (Monad, return)
import           Data.Data        (Data)
import           Data.Text        (Text, unpack)
import           Data.Typeable    (Typeable)
import           GHC.Generics     (Generic)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Base             as P
import qualified Prelude          as P
import           Show             (Print, putStrLn)

import           Applicative      (pass)

{-# WARNING trace "'trace' remains in code" #-}
trace :: Print b => b -> a -> a
trace string expr = unsafePerformIO (do
    putStrLn string
    return expr)

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

{-# WARNING traceId "'traceId' remains in code" #-}
traceId :: Text -> Text
traceId s = trace s s

{-# WARNING Undefined "'Undefined' type remains in code" #-}
data Undefined = Undefined
    deriving (P.Eq, P.Ord, P.Show, P.Read, P.Enum, P.Bounded, Data, Typeable, Generic)

{-# WARNING undefined "'undefined' function remains in code (or use 'error')" #-}
undefined :: a
undefined = P.undefined
