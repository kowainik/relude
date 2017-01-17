List
====

Slicing
-------

#### head

```haskell
head :: Foldable f => f a -> Maybe a
```

#### tailMay

```haskell
tailMay :: [a] -> Maybe [a]
```

#### tailSafe

```haskell
tailSafe :: [a] -> [a]
```

#### initMay

```haskell
initMay :: [a] -> Maybe [a]
```

#### initSafe

```haskell
initSafe :: [a] -> [a]
```

#### initDef

```haskell
initDef :: [a] -> [a] -> [a]
```

#### lastMay

```haskell
lastMay :: [a] -> Maybe a
```

#### lastDef

```haskell
lastDef :: a -> [a] -> a
```

#### drop

```haskell
drop :: Int -> [a] -> [a]
```

#### take

```haskell
take :: Int -> [a] -> [a]
```

Unpacking
---------

#### uncons

```haskell
uncons :: [a] -> Maybe (a, [a])
```

#### unsnoc

```haskell
unsnoc :: [x] -> Maybe ([x],x)
```

#### list

```haskell
list :: [b] -> (a -> b) -> [a] -> [b]
```

Sorting
---------

#### sortOn

```haskell
sortOn :: Ord o => (a -> o) -> [a] -> [a]
```

Removing
---------

#### ordNub

```haskell
ordNub :: Ord a => [a] -> [a]
```

Splitting
---------

#### splitAt

```haskell
splitAt :: Int -> [a] -> ([a], [a])
```

#### intercalate

```haskell
intercalate :: [a] -> [[a]] -> [a]
```

Comparison
---------

#### isPrefixOf

```haskell
isPrefixOf :: Eq a => [a] -> [a] -> Bool
```

Filtering
---------

#### filter

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

#### replicate

```haskell
replicate :: Int -> a -> [a]
```

Indexing
--------

#### atMay

```haskell
atMay :: [a] -> Int -> Maybe a
```

#### atDef

```haskell
atDef :: a -> [a] -> Int -> a
```
