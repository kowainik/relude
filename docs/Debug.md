Debug
=====

Stubbing
--------

#### undefined

```haskell
undefined :: a
```

An undefined expression standing in for an incomplete program, unevaluated type
witness, or unreachable code branch.

*Example*:

```haskell
> import Foreign.Storable 
> print (sizeOf (undefined :: Int))
8
```

#### notImplemented

```haskell
notImplemented :: a
```

An undefined expression standing in for a yet to completed program.

*Example*:

```haskell
main :: IO ()
main = notImplemented
```

Tracing
-------

#### trace

```haskell
trace :: Print b => b -> a -> a
```

*Example*:

#### traceM

```haskell
traceM :: (Monad m) => Text -> m ()
```

*Example*:

#### traceId

```haskell
traceId :: Text -> Text
```

*Example*:

#### traceShowM

```haskell
traceShowM :: (P.Show a, Monad m) => a -> m ()
```

*Example*:

#### traceShowId

```haskell
traceShowId :: P.Show a => a -> a
```

*Example*:

#### traceShow

```haskell
traceShow :: P.Show a => a -> b -> b
```

*Example*:

#### traceIO

```haskell
traceIO :: Print b => b -> a -> IO a
```

*Example*:
