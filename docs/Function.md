Functions
=========

```haskell
($) :: (a -> b) -> a -> b
```

```haskell
(&) :: a -> (a -> b) -> b
```

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

```haskell
flip :: (a -> b -> c) -> b -> a -> c
```

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```

```haskell
const :: a -> b -> a
```

```haskell
fix :: (a -> a) -> a
```

```haskell
($!) :: (a -> b) -> a -> b
```

```haskell
identity :: a -> a
```

```haskell
($!!) :: NFData a => (a -> b) -> a -> b
```

```haskell
($!!) :: NFData a => (a -> b) -> a -> b
```

```haskell
force :: NFData a => a -> a
```
