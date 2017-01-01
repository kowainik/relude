Bits
====

```haskell
{-# LANGUAGE BinaryLiterals #-}
```

```python
42 = 0b101010
```

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

```haskell
> True `xor` False
True

> True `xor` True
False

> 0b101 `xor` 0b011
6 -- 0b110
```

#### complement

```haskell
complement :: Bits a => a -> a
```

```haskell
> complement True
False

> complement 0b101
-2 -- -0b010
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

#### complementBit

```haskell
complementBit :: Bits a => a -> Int -> a
```

*Example*:

```haskell
λ> 0b100 `complementBit` 1
6 -- 0b110
```

#### testBit

```haskell
testBit :: Bits a => a -> Int -> Bool
```

*Example*:

```haskell
> 0b10 `testBit` 0
False
> 0b10 `testBit` 1
True
```

#### isSigned

```haskell
isSigned :: Bits a => a -> Bool
```

```haskell
> isSigned 42
True
> isSigned True
False
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
