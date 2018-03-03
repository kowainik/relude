{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | This module implements type class which allow to have conversion to and
-- from 'Text', 'String' and 'ByteString' types (including both strict and lazy
-- versions). Usually you need to export 'Text' modules qualified and use
-- 'T.pack' \/ 'T.unpack' functions to convert to\/from 'Text'. Now you can
-- just use 'toText' \/ 'toString' functions.

module Universum.String.Conversion
       ( -- * Convenient type aliases
         LText
       , LByteString

         -- * Conversion type classes
       , ConvertUtf8 (..)
       , ToString (..)
       , ToLText (..)
       , ToText (..)

         -- * Show and read functions
       , readEither
       , show
       , pretty
       , prettyL
       ) where

import Data.Bifunctor (first)
import Data.Either (Either)
import Data.Function (id, (.))
import Data.String (String)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (toLazyText)

import Universum.Functor ((<$>))
import Universum.String.Reexport (Buildable, ByteString, IsString, Read, Text, fromString, toStrict)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Text.Read (readEither)

import qualified GHC.Show as Show (Show (show))

-- $setup
-- >>> :set -XTypeApplications -XOverloadedStrings
-- >>> import Universum.Base (Int)
-- >>> import Universum.Function (($))
-- >>> import Universum.Print (putStrLn)

-- | Type synonym for 'Data.Text.Lazy.Text'.
type LText = LT.Text

-- | Type synonym for 'Data.ByteString.Lazy.ByteString'.
type LByteString = LB.ByteString


-- | Type class for conversion to utf8 representation of text.
class ConvertUtf8 a b where
    -- | Encode as utf8 string (usually 'B.ByteString').
    --
    -- >>> encodeUtf8 @Text @ByteString "патак"
    -- "\208\191\208\176\209\130\208\176\208\186"
    encodeUtf8 :: a -> b

    -- | Decode from utf8 string.
    --
    -- >>> decodeUtf8 @Text @ByteString "\208\191\208\176\209\130\208\176\208\186"
    -- "\1087\1072\1090\1072\1082"
    -- >>> putStrLn $ decodeUtf8 @Text @ByteString "\208\191\208\176\209\130\208\176\208\186"
    -- патак
    decodeUtf8 :: b -> a

    {- | Decode as utf8 string but returning execption if byte sequence is malformed.

#if MIN_VERSION_text(1,2,3)
    >>> decodeUtf8 @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    "\65533\1072\1090\1072\1082"
#else
    >>> decodeUtf8 @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    "\65533\65533\1090\1072\1082"
#endif
    >>> decodeUtf8Strict @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    Left Cannot decode byte '\xd0': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
    -}
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
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode . LB.fromStrict
    decodeUtf8Strict = decodeUtf8Strict . LB.fromStrict

instance ConvertUtf8 String LB.ByteString where
    encodeUtf8 = LBU.fromString
    decodeUtf8 = LBU.toString
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict

instance ConvertUtf8 T.Text LB.ByteString where
    encodeUtf8 = LB.fromStrict . T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With T.lenientDecode . LB.toStrict
    decodeUtf8Strict = T.decodeUtf8' . LB.toStrict

instance ConvertUtf8 LT.Text LB.ByteString where
    encodeUtf8 = LT.encodeUtf8
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode
    decodeUtf8Strict = LT.decodeUtf8'

-- | Type class for converting other strings to 'T.Text'.
class ToText a where
    toText :: a -> T.Text

instance ToText String where
    toText = T.pack

instance ToText T.Text where
    toText = id

instance ToText LT.Text where
    toText = LT.toStrict

-- | Type class for converting other strings to 'LT.Text'.
class ToLText a where
    toLText :: a -> LT.Text

instance ToLText String where
    toLText = LT.pack

instance ToLText T.Text where
    toLText = LT.fromStrict

instance ToLText LT.Text where
    toLText = id

-- | Type class for converting other strings to 'String'.
class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString T.Text where
    toString = T.unpack

instance ToString LT.Text where
    toString = LT.unpack

-- | Polymorhpic version of 'Text.Read.readEither'.
--
-- >>> readEither @Text @Int "123"
-- Right 123
-- >>> readEither @Text @Int "aa"
-- Left "Prelude.read: no parse"
readEither :: (ToString a, Read b) => a -> Either Text b
readEither = first toText . Text.Read.readEither . toString

-- | Generalized version of 'Prelude.show'.
show :: forall b a . (Show.Show a, IsString b) => a -> b
show x = fromString (Show.show x)
{-# SPECIALIZE show :: Show.Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> String  #-}

-- | Functions to show pretty output for buildable data types.
pretty :: Buildable a => a -> Text
pretty = toStrict . prettyL

-- | Similar to 'pretty' but for 'LText'.
prettyL :: Buildable a => a -> LText
prettyL = toLazyText . build
