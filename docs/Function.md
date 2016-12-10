Functions
=========

Composition
-----------

#### $

```haskell
($) :: (a -> b) -> a -> b
```

*Example*:

#### .

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

*Example*:

#### &

```haskell
(&) :: a -> (a -> b) -> b
```

*Example*:

#### flip

```haskell
flip :: (a -> b -> c) -> b -> a -> c
```

*Example*:

#### on

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```

*Example*:

#### const

```haskell
const :: a -> b -> a
```

*Example*:

#### fix

```haskell
fix :: (a -> a) -> a
```

*Example*:

#### identity

```haskell
identity :: a -> a
```

The identity function maps any value to itself.

#### applyN

Apply a function to a value `n` times.

*Example*:

```haskell
applyN :: Int -> (a -> a) -> a -> a
```

```haskell
> applyN 25 (+2) 0
50

> applyN 3 (1:) []
[1,1,1]
```

Strictness
-----------

#### $!

```haskell
($!) :: NFData a => (a -> b) -> a -> b
```

*Example*:

#### $!!

```haskell
($!!) :: NFData a => (a -> b) -> a -> b
```

*Example*:

#### force

```haskell
force :: NFData a => a -> a
```

*Example*:
