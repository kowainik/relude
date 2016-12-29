Functions
=========

Composition
-----------

#### $

```haskell
($) :: (a -> b) -> a -> b
```

Infix form of function application. Applies a function from ``a → b`` to an
argument ``a``.

*Example*:

```haskell
> take 2 $ [1,2,3]
[1,2]
```

#### .

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Function composition. Composes a function ``f`` (``b → c``)  with a function
``g`` (``a → b``) yielding ``f ∘ g``.

*Example*:

```haskell
> map (negate . abs) [-1,0,1]  
[-1,0,-1]
```

#### &

```haskell
(&) :: a -> (a -> b) -> b
```

Flipped form of ``($)`` which applies an argument ``a`` to a function ``a → b``.

*Example*:

```haskell
> [1,2,3] & take 2
[1,2]

> replicate 10 3 & take 5 & tail
[3,3,3,3]
```

#### flip

```haskell
flip :: (a -> b -> c) -> b -> a -> c
```

Flip takes a function of two arguments and returns a function taking the them in
reverse order.

*Example*:

```haskell
λ> flip take [1,2,3] 2
[1,2]
```

#### on

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```

*Example*:

```haskell
> sortBy (compare `on` fst) [(1,2), (3,4), (0,1)]
[(0,1),(1,2),(3,4)]
```

#### const

```haskell
const :: a -> b -> a
```

*Example*:

#### fix

```haskell
fix :: (a -> a) -> a
```

*Example*:

#### identity

```haskell
identity :: a -> a
```

The identity function maps any value to itself.

#### applyN

Apply a function to a value `n` times.

*Example*:

```haskell
applyN :: Int -> (a -> a) -> a -> a
```

```haskell
> applyN 25 (+2) 0
50

> applyN 3 (1:) []
[1,1,1]
```

Strictness
-----------

#### $!

```haskell
($!) :: NFData a => (a -> b) -> a -> b
```

*Example*:

#### $!!

```haskell
($!!) :: NFData a => (a -> b) -> a -> b
```

*Example*:

#### force

```haskell
force :: NFData a => a -> a
```

*Example*:
