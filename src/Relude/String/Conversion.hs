{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module implements type class which allow to have conversion to and from
'Text', 'String' and 'ByteString' types (including both strict and lazy
versions). Usually you need to export 'Text' modules qualified and use 'T.pack'
\/ 'T.unpack' functions to convert to\/from 'Text'. Now you can just use
'toText' \/ 'toString' functions.
-}

module Relude.String.Conversion
       ( -- * Convenient type aliases
         LText
       , LByteString

         -- * Conversion type classes
       , ConvertUtf8 (..)
       , ToString (..)
       , ToLText (..)
       , ToText (..)
       , LazyStrict (..)
       , fromLazy
       , fromStrict

         -- * Show and read functions
       , readEither
       , show
       ) where

import Data.Bifunctor (first)
import Data.Either (Either)
import Data.Function (id, (.))
import Data.String (String)

import Relude.Functor ((<$>))
import Relude.String.Reexport (ByteString, IsString, Read, ShortByteString, Text, fromShort,
                               fromString, toShort)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Text.Read (readEither)

import qualified GHC.Show as Show (Show (show))

-- $setup
-- >>> :set -XTypeApplications -XOverloadedStrings
-- >>> import Relude

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
    -- >>> putTextLn $ decodeUtf8 @Text @ByteString "\208\191\208\176\209\130\208\176\208\186"
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

instance ConvertUtf8 String ByteString where
    encodeUtf8 :: String -> ByteString
    encodeUtf8 = T.encodeUtf8 . T.pack
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ByteString -> String
    decodeUtf8 = T.unpack . T.decodeUtf8
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ByteString -> Either T.UnicodeException String
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict
    {-# INLINE decodeUtf8Strict #-}

instance ConvertUtf8 Text ByteString where
    encodeUtf8 :: Text -> ByteString
    encodeUtf8 = T.encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ByteString -> Text
    decodeUtf8 = T.decodeUtf8With T.lenientDecode
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ByteString -> Either T.UnicodeException Text
    decodeUtf8Strict = T.decodeUtf8'
    {-# INLINE decodeUtf8Strict #-}

instance ConvertUtf8 LText ByteString where
    encodeUtf8 :: LText -> ByteString
    encodeUtf8 = LB.toStrict . encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ByteString -> LText
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode . LB.fromStrict
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ByteString -> Either T.UnicodeException LText
    decodeUtf8Strict = decodeUtf8Strict . LB.fromStrict
    {-# INLINE decodeUtf8Strict #-}

-- | Converting 'String' to 'LB.ByteString' might be a slow operation.
-- Consider using lazy bytestring at first place.
instance ConvertUtf8 String LByteString where
    encodeUtf8 :: String -> LByteString
    encodeUtf8 = LT.encodeUtf8 . LT.pack
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: LByteString -> String
    decodeUtf8 = LT.unpack . LT.decodeUtf8
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: LByteString -> Either T.UnicodeException String
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict
    {-# INLINE decodeUtf8Strict #-}

instance ConvertUtf8 Text LByteString where
    encodeUtf8 :: Text -> LByteString
    encodeUtf8 = LB.fromStrict . T.encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: LByteString -> Text
    decodeUtf8 = T.decodeUtf8With T.lenientDecode . LB.toStrict
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: LByteString -> Either T.UnicodeException Text
    decodeUtf8Strict = T.decodeUtf8' . LB.toStrict
    {-# INLINE decodeUtf8Strict #-}

instance ConvertUtf8 LText LByteString where
    encodeUtf8 :: LText -> LByteString
    encodeUtf8 = LT.encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: LByteString -> LText
    decodeUtf8 = LT.decodeUtf8With T.lenientDecode
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: LByteString -> Either T.UnicodeException LText
    decodeUtf8Strict = LT.decodeUtf8'
    {-# INLINE decodeUtf8Strict #-}

instance ConvertUtf8 String ShortByteString where
    encodeUtf8 :: String -> ShortByteString
    encodeUtf8 = toShort . T.encodeUtf8 . T.pack
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ShortByteString -> String
    decodeUtf8 = T.unpack . T.decodeUtf8 . fromShort
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ShortByteString -> Either T.UnicodeException String
    decodeUtf8Strict = (T.unpack <$>) . decodeUtf8Strict . fromShort
    {-# INLINE decodeUtf8Strict #-}

instance ConvertUtf8 Text ShortByteString where
    encodeUtf8 :: Text -> ShortByteString
    encodeUtf8 = toShort . T.encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ShortByteString -> Text
    decodeUtf8 = T.decodeUtf8With T.lenientDecode . fromShort
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ShortByteString -> Either T.UnicodeException Text
    decodeUtf8Strict = T.decodeUtf8' . fromShort
    {-# INLINE decodeUtf8Strict #-}

-- | Type class for converting other strings to 'T.Text'.
class ToText a where
    toText :: a -> Text

instance ToText String where
    toText :: String -> Text
    toText = T.pack
    {-# INLINE toText #-}

instance ToText Text where
    toText :: Text -> Text
    toText = id
    {-# INLINE toText #-}

instance ToText LText where
    toText :: LText -> Text
    toText = LT.toStrict
    {-# INLINE toText #-}

-- | Type class for converting other strings to 'LT.Text'.
class ToLText a where
    toLText :: a -> LText

instance ToLText String where
    toLText :: String -> LText
    toLText = LT.pack
    {-# INLINE toLText #-}

instance ToLText Text where
    toLText :: Text -> LText
    toLText = LT.fromStrict
    {-# INLINE toLText #-}

instance ToLText LT.Text where
    toLText :: LText -> LText
    toLText = id
    {-# INLINE toLText #-}

-- | Type class for converting other strings to 'String'.
class ToString a where
    toString :: a -> String

instance ToString String where
    toString :: String -> String
    toString = id
    {-# INLINE toString #-}

instance ToString Text where
    toString :: Text -> String
    toString = T.unpack
    {-# INLINE toString #-}

instance ToString LText where
    toString :: LText -> String
    toString = LT.unpack
    {-# INLINE toString #-}

-- | Polymorhpic version of 'Text.Read.readEither'.
--
-- >>> readEither @Text @Int "123"
-- Right 123
-- >>> readEither @Text @Int "aa"
-- Left "Prelude.read: no parse"
readEither :: (ToString a, Read b) => a -> Either Text b
readEither = first toText . Text.Read.readEither . toString
{-# INLINEABLE readEither #-}

-- | Generalized version of 'Prelude.show'.
show :: forall b a . (Show.Show a, IsString b) => a -> b
show x = fromString (Show.show x)
{-# INLINE show #-}
{-# SPECIALIZE show :: Show.Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> String  #-}

-- | Type class for lazy-strict conversions.
class LazyStrict l s | l -> s, s -> l where
    toLazy   :: s -> l
    toStrict :: l -> s

-- | Alias for 'toStrict' function.
fromLazy :: LazyStrict l s => l -> s
fromLazy = toStrict
{-# INLINE fromLazy #-}
{-# SPECIALIZE fromLazy :: LByteString -> ByteString  #-}
{-# SPECIALIZE fromLazy :: LText -> Text  #-}

-- | Alias for 'toLazy' function.
fromStrict :: LazyStrict l s => s -> l
fromStrict = toLazy
{-# INLINE fromStrict #-}
{-# SPECIALIZE fromStrict :: ByteString -> LByteString  #-}
{-# SPECIALIZE fromStrict :: Text -> LText  #-}

instance LazyStrict LByteString ByteString where
    toLazy :: ByteString -> LByteString
    toLazy = LB.fromStrict
    {-# INLINE toLazy #-}

    toStrict :: LByteString -> ByteString
    toStrict = LB.toStrict
    {-# INLINE toStrict #-}

instance LazyStrict LText Text where
    toLazy :: Text -> LText
    toLazy = LT.fromStrict
    {-# INLINE toLazy #-}

    toStrict :: LText -> Text
    toStrict = LT.toStrict
    {-# INLINE toStrict #-}
