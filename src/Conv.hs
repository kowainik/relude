{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Conv
       ( ConvertUtf8 (..)
       , ToString (..)
       , ToLText (..)
       , ToText (..)
       ) where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lazy.UTF8  as LBU
import qualified Data.ByteString.UTF8       as BU
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT

import           Control.Applicative        (pure)
import           Data.Either                (Either)
import           Data.Function              (id, (.))
import           Data.String                (String)
import           Functor                    ((<$>))

class ConvertUtf8 a b where
    encodeUtf8 :: a -> b
    decodeUtf8 :: b -> a
    decodeUtf8Strict :: b -> Either T.UnicodeException a

instance ConvertUtf8 String B.ByteString where
    encodeUtf8 = BU.fromString
    decodeUtf8 = BU.toString
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict

instance ConvertUtf8 T.Text B.ByteString where
    encodeUtf8 = T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With T.lenientDecode
    decodeUtf8Strict = T.decodeUtf8'

instance ConvertUtf8 LT.Text B.ByteString where
    encodeUtf8 = LB.toStrict . encodeUtf8
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode . LB.fromChunks . pure
    decodeUtf8Strict = decodeUtf8Strict . LB.fromStrict

instance ConvertUtf8 String LB.ByteString where
    encodeUtf8 = LBU.fromString
    decodeUtf8 = LBU.toString
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict

instance ConvertUtf8 T.Text LB.ByteString where
    encodeUtf8 = LB.fromStrict . T.encodeUtf8
    decodeUtf8 = decodeUtf8
    decodeUtf8Strict = T.decodeUtf8' . LB.toStrict

instance ConvertUtf8 LT.Text LB.ByteString where
    encodeUtf8 = LT.encodeUtf8
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode
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
