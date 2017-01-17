Applicative
===========

Functor
-------

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
```

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

```haskell
($>) :: Functor f => f a -> b -> f b
```

Applicatives 
-------

```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
```

```haskell
orAlt :: (Alternative f, Monoid a) => f a -> f a
```

```haskell
orEmpty :: Alternative f => Bool -> a -> f a
```

```haskell
eitherA :: (Alternative f) => f a -> f b -> f (Either a b)
```

```haskell
pass :: Applicative f => f ()
```


Alternative 
-------

```haskell
class Applicative f => Alternative (f :: * -> *) where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
```

```haskell
(<|>) :: Alternative f => f a -> f a -> f a
```

```haskell
many :: Alternative f => f a -> f [a]
```

```haskell
some :: Alternative f => f a -> f [a]
```

```haskell
optional :: Alternative f => f a -> f (Maybe a)
```

```haskell
liftA :: Applicative f => (a -> b) -> f a -> f b
```

```haskell
empty :: Alternative f => f a
```

```haskell
guarded :: (Alternative f) => (a -> Bool) -> a -> f a
```

```haskell
guardedA :: (Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
```
