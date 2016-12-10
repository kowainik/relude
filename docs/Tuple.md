Tuples
======

#### fst

```haskell
fst :: (a, b) -> a
```

Extract the first component of a pair.

*Example*:

```haskell
> fst (1,2)
```

#### snd

```haskell
snd :: (a, b) -> b
```

Extract the second component of a pair.

*Example*:

```haskell
> snd (1,2)
2
```

#### swap

```haskell
swap :: (a, b) -> (b, a)
```

Swap the components of a pair.

*Example*:

```haskell
> swap (1,2)
(2,1)
```

#### curry

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
```

curry converts an uncurried function to a curried function.

*Example*:

```haskell
> curry fst 1 2
1
```

#### uncurry

```haskell
uncurry :: (a -> b -> c) -> (a, b) -> c
```

uncurry converts a curried function to a function on pairs.

*Example*:

```haskell
> uncurry (+) (1,2)
3
```
