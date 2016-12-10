Monads
======

Monad
-----

```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
```

```haskell
(>>) :: Monad m => m a -> m b -> m b
```

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

```haskell
forever :: Monad m => m a -> m b
```

```haskell
join :: Monad m => m (m a) -> m a
```

```haskell
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
```

```haskell
mapAndUnzipM :: Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])
```

```haskell
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
```

```haskell
zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
```

```haskell
foldM :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
```

```haskell
foldM_ :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m ()
```

```haskell
replicateM :: Monad m => Int -> m a -> m [a]
```

```haskell
replicateM_ :: Monad m => Int -> m a -> m ()
```

```haskell
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
```

```haskell
guard :: Alternative f => Bool -> f ()
```

```haskell
when :: Applicative f => Bool -> f () -> f ()
```

```haskell
unless :: Applicative f => Bool -> f () -> f ()
```

```haskell
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
```

```haskell
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
```

```haskell
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
```

```haskell
liftM4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
```

```haskell
liftM5 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
```

```haskell
liftM' :: Monad m => (a -> b) -> m a -> m b
```

```haskell
liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
```

```haskell
ap :: Monad m => m (a -> b) -> m a -> m b
```

```haskell
(<$!>) :: Monad m => (a -> b) -> m a -> m b
```

```haskell
whenM :: Monad m => m Bool -> m () -> m ()
```

```haskell
unlessM :: Monad m => m Bool -> m () -> m ()
```

```haskell
ifM :: Monad m => m Bool -> m a -> m a -> m a
```

```haskell
guardM :: MonadPlus m => m Bool -> m ()
```

MonadPlus
-----

```haskell
class (Alternative m, Monad m) => MonadPlus (m :: * -> *) where
  mzero :: m a
  mplus :: m a -> m a -> m a
  	-- Defined in ‘GHC.Base’
```

```haskell
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
```
