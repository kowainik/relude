Maybe
=====

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
```

```haskell
isJust :: Maybe a -> Bool
```

```haskell
isNothing :: Maybe a -> Bool
```

```haskell
fromJust :: Maybe a -> a
```

```haskell
fromMaybe :: a -> Maybe a -> a
```

```haskell
listToMaybe :: [a] -> Maybe a
```

```haskell
maybeToList :: Maybe a -> [a]
```

```haskell
catMaybes :: [Maybe a] -> [a]
```

```haskell
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
```
