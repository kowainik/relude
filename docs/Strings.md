Strings
=======

```haskell
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
```

Text
----

The Text type represents Unicode character strings, in a time and space-efficient manner. This package provides text processing capabilities that are optimized for performance critical use, both in terms of large data quantities and high speed.

LText
-----

Bytestring
----------

LBytestring
-----------

Encoding
----------

#### encodeUtf8

```haskell
encodeUtf8 :: Text -> ByteString
```

```haskell
> encodeUtf8 "ポケット"
"\227\131\157\227\130\177\227\131\131\227\131\136"
```

#### decodeUtf8

```haskell
decodeUtf8 :: ByteString -> Text
```

```haskell
> putStrLn $ decodeUtf8 "\227\131\157\227\130\177\227\131\131\227\131\136"
ポケット
```

#### decodeUtf8'

```haskell
decodeUtf8' :: ByteString -> Either UnicodeException Text
```

#### decodeUtf8With

```haskell
decodeUtf8With :: OnDecodeError -> ByteString -> Text
```

Conversion
----------

```haskell
class StringConv a b where
  strConv :: Leniency -> a -> b

data Leniency = Lenient | Strict
```

```haskell
toS :: StringConv a b => a -> b
```

```haskell
toSL :: StringConv a b => a -> b
```

*Example*:

```haskell
a :: LByteString
a = "Einstein"

b :: Text
b = "Feynmann"

c :: ByteString
c = "Schrödinger"

example1 :: ByteString
example1 = toS b

example2 :: Bool
example2 = (a == toS b) && (toS b == c)
```
