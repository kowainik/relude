{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Conv
       ( ConvertUtf8 (..)
       , ToString (..)
       , ToLText (..)
       , ToText (..)
       , Leniency (..)
       ) where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT

import           Base
import           Bifunctor                  (bimap)
import           Control.Applicative        (pure)
import           Data.Either                (Either)
import           Data.Eq                    (Eq (..))
import           Data.Function              (id, (.))
import           Data.Ord                   (Ord (..))
import           Data.String                (String)

data Leniency = Lenient | Strict
    deriving (Eq,Show,Ord,Enum,Bounded)

class ConvertUtf8 a b where
    encodeUtf8 :: a -> b
    decodeUtf8 :: b -> a
    decodeUtf8Strict :: b -> Either T.UnicodeException a

instance ConvertUtf8 String B.ByteString where
    encodeUtf8 = B.pack
    decodeUtf8 = B.unpack
    decodeUtf8Strict = bimap id T.unpack . decodeUtf8Strict

instance ConvertUtf8 T.Text B.ByteString where
    encodeUtf8 = T.encodeUtf8
    decodeUtf8 = decodeUtf8T Lenient
    decodeUtf8Strict = T.decodeUtf8'

instance ConvertUtf8 LT.Text B.ByteString where
    encodeUtf8 = LB.toStrict . encodeUtf8
    decodeUtf8 = decodeUtf8LT Lenient . LB.fromChunks . pure
    decodeUtf8Strict = decodeUtf8Strict . LB.fromChunks . pure

instance ConvertUtf8 String LB.ByteString where
    encodeUtf8 = LB.pack
    decodeUtf8 = LB.unpack
    decodeUtf8Strict = bimap id T.unpack . decodeUtf8Strict

instance ConvertUtf8 T.Text LB.ByteString where
    encodeUtf8 = LB.fromStrict . T.encodeUtf8
    decodeUtf8 = decodeUtf8
    decodeUtf8Strict = T.decodeUtf8' . LB.toStrict

instance ConvertUtf8 LT.Text LB.ByteString where
    encodeUtf8 = LT.encodeUtf8
    decodeUtf8 = decodeUtf8LT Lenient
    decodeUtf8Strict = LT.decodeUtf8'

class ToText a where
    toText :: a -> T.Text

instance ToText String where
    toText = T.pack

instance ToText T.Text where
    toText = id

instance ToText LT.Text where
    toText = LT.toStrict

class ToLText a where
    toLText :: a -> LT.Text

instance ToLText String where
    toLText = LT.pack

instance ToLText T.Text where
    toLText = LT.fromStrict

instance ToLText LT.Text where
    toLText = id

class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString T.Text where
    toString = T.unpack

instance ToString LT.Text where
    toString = LT.unpack

decodeUtf8T :: Leniency -> B.ByteString -> T.Text
decodeUtf8T Lenient = T.decodeUtf8With T.lenientDecode
decodeUtf8T Strict  = T.decodeUtf8With T.strictDecode

decodeUtf8LT :: Leniency -> LB.ByteString -> LT.Text
decodeUtf8LT Lenient = LT.decodeUtf8With T.lenientDecode
decodeUtf8LT Strict  = LT.decodeUtf8With T.strictDecode
