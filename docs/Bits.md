Bits
====

Bit Operations
--------------

#### .&.

```haskell
(.&.) :: Bits a => a -> a -> a
```

*Example*:

#### .|.

```haskell
(.|.) :: Bits a => a -> a -> a
```

*Example*:

#### xor

```haskell
xor :: Bits a => a -> a -> a
```

*Example*:

#### complement

```haskell
complement :: Bits a => a -> a
```

*Example*:

#### shift

*Example*:

```haskell
shift :: Bits a => a -> Int -> a
```

*Example*:

#### rotate

```haskell
rotate :: Bits a => a -> Int -> a
```

*Example*:

#### zeroBits

```haskell
zeroBits :: Bits a => a
```

*Example*:

#### bit

```haskell
bit :: Bits a => Int -> a
```

*Example*:

Bit Testing
------------

```haskell
setBit :: Bits a => a -> Int -> a
```

```haskell
clearBit :: Bits a => a -> Int -> a
```

```haskell
complementBit :: Bits a => a -> Int -> a
```

```haskell
testBit :: Bits a => a -> Int -> Bool
```


```haskell
isSigned :: Bits a => a -> Bool
```

Bit Size
------------

```haskell
bitSize :: Bits a => a -> Int
```

```haskell
popCount :: Bits a => a -> Int
```

Bit Shifting
------------

```haskell
shiftL :: Bits a => a -> Int -> a
```

```haskell
shiftR :: Bits a => a -> Int -> a
```

Bit Rotation
------------

```haskell
rotate :: Bits a => a -> Int -> a
```

```haskell
rotateL :: Bits a => a -> Int -> a
```

```haskell
rotateR :: Bits a => a -> Int -> a
```

Byte Swapping
------------

```haskell
byteSwap16 :: Word16 -> Word16
```

```haskell
byteSwap32 :: Word32 -> Word32
```

```haskell
byteSwap64 :: Word64 -> Word64
```
