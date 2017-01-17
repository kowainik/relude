Hashing
=======

```haskell
hashWithSalt :: Hashable a => Int -> a -> Int
```

```haskell
hash :: Hashable a => a -> Int
```

```haskell
hashUsing :: Hashable b => (a -> b) -> Int -> a -> Int
```
