Monoid
======

Monoid
------

Semigroup
---------


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
