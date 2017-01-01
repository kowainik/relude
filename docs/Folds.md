Folds
=====

Basic Folds
-----------

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

```haskell
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

```haskell
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

```haskell
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

```haskell
fold :: Foldable t => Monoid m => t m -> m
```

```haskell
toList :: Foldable t => t a -> [a]
```

```haskell
null :: Foldable t => t a -> Bool
```

```haskell
length :: Foldable t => t a -> Int
```

```haskell
elem :: (Eq a, Foldable t) => a -> t a -> Bool
```

```haskell
maximum :: (Ord a, Foldable t) => t a -> a
```

```haskell
minimum :: (Ord a, Foldable t) => t a -> a
```

```haskell
sum :: (Num a, Foldable f) => f a -> a
```

```haskell
product :: (Num a, Foldable f) => f a -> a
```

#### Folding actions

```haskell
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
```

```haskell
for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
```

*Example*:

```haskell
>>> for_ [1..4] print
1
2
3
4
```


#### Applicative Folds

```haskell
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
```

```haskell
asum :: (Foldable t, Alternative f) => t (f a) -> f a
```

#### Monadic Folds

```haskell
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
```

```haskell
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
```

```haskell
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
```

```haskell
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
```

```haskell
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
```

```haskell
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
```

#### Specialized folds

```haskell
concat :: Foldable t => t [a] -> [a]
```

```haskell
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```

```haskell
and :: Foldable t => t Bool -> Bool
```

```haskell
or :: Foldable t => t Bool -> Bool
```

```haskell
any :: Foldable t => (a -> Bool) -> t a -> Bool
```

```haskell
all :: Foldable t => (a -> Bool) -> t a -> Bool
```

```haskell
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
```

```haskell
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
```

#### Searches

```haskell
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
```

```haskell
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
```


```haskell
foldr1May :: (a -> a -> a) -> [a] -> Maybe a
```

```haskell
foldl1May :: (a -> a -> a) -> [a] -> Maybe a
```
