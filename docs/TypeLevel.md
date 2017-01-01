Type Level
==========

```haskell
data Coercion (a :: k) (b :: k) where
  Coercion :: forall (k :: BOX) (a :: k) (b :: k).  Coercible a b => Coercion a b
```

```haskell
coerceWith :: Coercion a b -> a -> b
```

#### Empty

```haskell
data Void
```

```haskell
absurd :: Void -> a
```

```haskell
vacuous :: Functor f => f Void -> f a
```

#### Proxy

```haskell
data Proxy (t :: k) = Proxy
```

#### Symbol

```haskell
symbolVal :: KnownSymbol n => proxy n -> String
```

*Example*:

```haskell
b :: String
b = symbolVal (Proxy :: Proxy "foo")
```

```haskell
someSymbolVal :: String -> SomeSymbol
```

*Example*:

#### Nat

```haskell
natVal :: KnownNat n => proxy n -> Integer
```

*Example*:

```haskell
a :: Integer
a = natVal (Proxy :: Proxy 1)
```

```haskell
someNatVal :: Integer -> Maybe SomeNat
```

*Example*:

#### Type Equality

```haskell
(:~:) :: k -> k -> *
```

```haskell
(==) :: k -> k -> Bool
```

```haskell
sym :: a :~: b -> b :~: a
```

```haskell
trans :: a :~: b -> b :~: c -> a :~: c
```

```haskell
castWith :: a :~: b -> a -> b
```

```haskell
gcastWith :: a :~: b -> ((a ~ b) => r) -> r
```
