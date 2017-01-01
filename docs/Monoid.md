Monoid
======

Monoid
------

#### mempty

```haskell
mempty :: Monoid a => a
```

#### <>

```haskell
(<>) :: Monoid m => m -> m -> m
```

```haskell
mappend :: Monoid a => a -> a -> a
```

#### mconcat

```haskell
mconcat :: Monoid a => [a] -> a
```

Semigroup
---------

#### <>

```haskell
(<>) :: Semigroup a => a -> a -> a
```

#### sconcat

```haskell
sconcat :: Semigroup a => NonEmpty a -> a
```

#### stimes

```haskell
stimes :: (Semigroup a, Integral b) => b -> a -> a
```

```haskell
option :: b -> (a -> b) -> Option a -> b
```

```haskell
diff :: Semigroup m => m -> Endo m
```

```haskell
cycle1 :: Semigroup m => m -> m
```

```haskell
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
```

```haskell
stimesIdempotent :: Integral b => b -> a -> a
```

```haskell
stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
```

```haskell
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
```

NonEmpty
---------
