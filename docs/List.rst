List
====

#### Slicing

```haskell
head :: Foldable f => f a -> Maybe a
```

```haskell
tailMay :: [a] -> Maybe [a]
```

```haskell
tailSafe :: [a] -> [a]
```

```haskell
initMay :: [a] -> Maybe [a]
```

```haskell
initSafe :: [a] -> [a]
```

```haskell
initDef :: [a] -> [a] -> [a]
```

```haskell
lastMay :: [a] -> Maybe a
```

```haskell
lastDef :: a -> [a] -> a
```

```haskell
list :: [b] -> (a -> b) -> [a] -> [b]
```

```haskell
drop :: Int -> [a] -> [a]
```

```haskell
take :: Int -> [a] -> [a]
```

#### Unpacking

```haskell
uncons :: [a] -> Maybe (a, [a])
```

```haskell
unsnoc :: [x] -> Maybe ([x],x)
```

#### Sorting

```haskell
sortOn :: Ord o => (a -> o) -> [a] -> [a]
```

#### Removing

```haskell
ordNub :: Ord a => [a] -> [a]
```

#### Splitting

```haskell
splitAt :: Int -> [a] -> ([a], [a])
```

```haskell
splitAt :: Int -> [a] -> ([a], [a])
```

```haskell
intercalate :: [a] -> [[a]] -> [a]
```

#### Comparison

```haskell
isPrefixOf :: Eq a => [a] -> [a] -> Bool
```

#### Filter

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

```haskell
replicate :: Int -> a -> [a]
```

```haskell
map :: Functor f => (a -> b) -> f a -> f b
```
