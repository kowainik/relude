{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

{- |
Module                  : Relude.String.Conversion
Copyright               : (c) 2016 Stephen Diehl
                          (c) 2016-2018 Serokell
                          (c) 2018-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module implements type class which allow to have conversion to and from
'Relude.String.Reexport.Text', 'String' and 'ByteString' types
(including both strict and lazy versions). Usually you need to export
'Relude.String.Reexport.Text' modules qualified and use 'T.pack' \/ 'T.unpack'
functions to convert to\/from 'Relude.String.Reexport.Text'. Now you can just
use 'toText' \/ 'toString' functions.
-}

module Relude.String.Conversion
    ( -- * Convenient type aliases
      LText
    , LByteString

      -- * Conversion type classes
    , ConvertUtf8 (..)
    , ToText (..)
    , ToLText (..)
    , ToString (..)
    , LazyStrict (..)
    , fromLazy
    , fromStrict

      -- * Show and read functions
    , readEither
    , show
    ) where

import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import Prelude (error)

import Relude.Base (Constraint, Type)
import Relude.Function (id, (.))
import Relude.Functor (first, (<$>))
import Relude.Monad.Reexport (Either)
import Relude.String.Reexport (ByteString, IsString, Read, ShortByteString, String, Text, fromShort,
                               fromString, toShort)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified GHC.Show as Show (Show (show))
import qualified Text.Read (readEither)


-- $setup
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

#if MIN_VERSION_text(2,0,0)
    >>> decodeUtf8Strict @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    Left Cannot decode byte '\xd0': Data.Text.Internal.Encoding: Invalid UTF-8 stream
#else
    >>> decodeUtf8Strict @Text @ByteString "\208\208\176\209\130\208\176\208\186"
    Left Cannot decode byte '\xd0': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
#endif
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

{- | Converting 'String' to 'LB.ByteString' might be a slow operation.
Consider using lazy bytestring at first place.
-}
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

-- | @since 0.6.0.0
instance ConvertUtf8 String ShortByteString where
    encodeUtf8 :: String -> ShortByteString
    encodeUtf8 = toShort . encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ShortByteString -> String
    decodeUtf8 = decodeUtf8 . fromShort
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ShortByteString -> Either T.UnicodeException String
    decodeUtf8Strict = decodeUtf8Strict . fromShort
    {-# INLINE decodeUtf8Strict #-}

-- | @since 0.6.0.0
instance ConvertUtf8 Text ShortByteString where
    encodeUtf8 :: Text -> ShortByteString
    encodeUtf8 = toShort . encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ShortByteString -> Text
    decodeUtf8 = decodeUtf8 . fromShort
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ShortByteString -> Either T.UnicodeException Text
    decodeUtf8Strict = decodeUtf8Strict . fromShort
    {-# INLINE decodeUtf8Strict #-}

-- | @since 0.6.0.0
instance ConvertUtf8 LText ShortByteString where
    encodeUtf8 :: LText -> ShortByteString
    encodeUtf8 = toShort . encodeUtf8
    {-# INLINE encodeUtf8 #-}

    decodeUtf8 :: ShortByteString -> LText
    decodeUtf8 = decodeUtf8 . fromShort
    {-# INLINE decodeUtf8 #-}

    decodeUtf8Strict :: ShortByteString -> Either T.UnicodeException LText
    decodeUtf8Strict = decodeUtf8Strict . fromShort
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

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toText ("some string" :: ByteString)
...
... Type 'ByteString' doesn't have instance of 'ToText'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: ByteString -> Text
          decodeUtf8Strict :: ByteString -> Either UnicodeException Text
...

@since 0.6.0.0
-}
instance EncodingError ToText "ByteString" "Text" => ToText ByteString where
    toText :: ByteString -> Text
    toText = error "Unreachable ByteString instance of ToText"

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toText ("some string" :: LByteString)
...
... Type 'LByteString' doesn't have instance of 'ToText'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: LByteString -> Text
          decodeUtf8Strict :: LByteString -> Either UnicodeException Text
...

@since 0.6.0.0
-}
instance EncodingError ToText "LByteString" "Text" => ToText LByteString where
    toText :: LByteString -> Text
    toText = error "Unreachable LByteString instance of ToText"

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toText ("some string" :: ShortByteString)
...
... Type 'ShortByteString' doesn't have instance of 'ToText'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: ShortByteString -> Text
          decodeUtf8Strict :: ShortByteString -> Either UnicodeException Text
...

@since 0.6.0.0
-}
instance EncodingError ToText "ShortByteString" "Text" => ToText ShortByteString where
    toText :: ShortByteString -> Text
    toText = error "Unreachable ShortByteString instance of ToText"

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

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toLText ("some string" :: ByteString)
...
... Type 'ByteString' doesn't have instance of 'ToLText'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: ByteString -> LText
          decodeUtf8Strict :: ByteString -> Either UnicodeException LText
...

@since 0.6.0.0
-}
instance EncodingError ToLText "ByteString" "LText" => ToLText ByteString where
    toLText :: ByteString -> LText
    toLText = error "Unreachable ByteString instance of ToLText"

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toLText ("some string" :: LByteString)
...
... Type 'LByteString' doesn't have instance of 'ToLText'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: LByteString -> LText
          decodeUtf8Strict :: LByteString -> Either UnicodeException LText
...

@since 0.6.0.0
-}
instance EncodingError ToLText "LByteString" "LText" => ToLText LByteString where
    toLText :: LByteString -> LText
    toLText = error "Unreachable LByteString instance of ToLText"

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toLText ("some string" :: ShortByteString)
...
... Type 'ShortByteString' doesn't have instance of 'ToLText'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: ShortByteString -> LText
          decodeUtf8Strict :: ShortByteString -> Either UnicodeException LText
...

@since 0.6.0.0
-}
instance EncodingError ToLText "ShortByteString" "LText" => ToLText ShortByteString where
    toLText :: ShortByteString -> LText
    toLText = error "Unreachable ShortByteString instance of ToLText"

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

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toString ("some string" :: ByteString)
...
... Type 'ByteString' doesn't have instance of 'ToString'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: ByteString -> String
          decodeUtf8Strict :: ByteString -> Either UnicodeException String
...

@since 0.6.0.0
-}
instance EncodingError ToString "ByteString" "String" => ToString ByteString where
    toString :: ByteString -> String
    toString = error "Unreachable ByteString instance of ToString"

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toString ("some string" :: LByteString)
...
... Type 'LByteString' doesn't have instance of 'ToString'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: LByteString -> String
          decodeUtf8Strict :: LByteString -> Either UnicodeException String
...

@since 0.6.0.0
-}
instance EncodingError ToString "LByteString" "String" => ToString LByteString where
    toString :: LByteString -> String
    toString = error "Unreachable LByteString instance of ToString"

{- | ⚠️__CAUTION__⚠️ This instance is for custom error display only.

You should always specify encoding of bytes explicitly.

In case it is used by mistake, the user will see the following:

>>> toString ("some string" :: ShortByteString)
...
... Type 'ShortByteString' doesn't have instance of 'ToString'.
      Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:
          decodeUtf8       :: ShortByteString -> String
          decodeUtf8Strict :: ShortByteString -> Either UnicodeException String
...

@since 0.6.0.0
-}
instance EncodingError ToString "ShortByteString" "String" => ToString ShortByteString where
    toString :: ShortByteString -> String
    toString = error "Unreachable ShortByteString instance of ToString"

-- | Helper type family to produce error messages
type family EncodingError
    (c :: Type -> Constraint)
    (from :: Symbol)
    (to :: Symbol)
    :: Constraint
  where
    EncodingError c from to = TypeError
        ( 'Text "Type '" ':<>: 'Text from ':<>: 'Text "' doesn't have instance of '"
        ':<>: 'ShowType c ':<>: 'Text "'."
        ':$$: 'Text "Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8:"
        ':$$: 'Text "    decodeUtf8       :: " ':<>: 'Text from
        ':<>: 'Text " -> " ':<>: 'Text to
        ':$$: 'Text "    decodeUtf8Strict :: " ':<>: 'Text from
        ':<>: 'Text " -> Either UnicodeException " ':<>: 'Text to
        )

{- | Version of 'Text.Read.readEither' that returns 'Text' in case of the parse
error.

>>> readEither @Int "123"
Right 123
>>> readEither @Int "aa"
Left "Prelude.read: no parse"
-}
readEither :: (Read a) => String -> Either Text a
readEither = first toText . Text.Read.readEither
{-# INLINEABLE readEither #-}

{- | Generalized version of 'Prelude.show'. Unlike 'Prelude.show' this function
is polymorphic in its result type. This makes it more convenient to work with
data types like 'Relude.String.Reexport.Text' or 'ByteString'. However, if you
pass the result of 'show' to a function that expects polymorphic argument, this
can break type inference, so use @-XTypeApplications@ to specify the textual
type explicitly.

>>> show (42 :: Int)
"42"
>>> show (42 :: Double)
"42.0"
>>> print (show @Text True)
"True"
-}
show :: forall b a . (Show.Show a, IsString b) => a -> b
show x = fromString (Show.show x)
{-# INLINE show #-}
{-# SPECIALIZE show :: Show.Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show.Show  a => a -> String  #-}

{- | Type class for lazy-strict conversions.

@since 0.1.0
-}
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
