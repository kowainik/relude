Functor
=======

#### map

```haskell
map :: Functor f => (a -> b) -> f a -> f b
```

#### $>

```haskell
($>) :: Functor f => f a -> b -> f b
```

#### <$>

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

####  <<$>>

```haskell
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
```

#### void

```haskell
void :: Functor f => f a -> f ()
```

#### foreach

```haskell
foreach :: Functor f => f a -> (a -> b) -> f b
```


