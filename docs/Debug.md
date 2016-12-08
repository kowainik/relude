Debug
=====

```haskell
undefined :: a
```

```haskell
notImplemented :: a
```

```haskell
trace :: Print b => b -> a -> a
```

```haskell
traceM :: (Monad m) => Text -> m ()
```

```haskell
traceId :: Text -> Text
```

```haskell
traceShowM :: (P.Show a, Monad m) => a -> m ()
```

```haskell
traceShowId :: P.Show a => a -> a
```

```haskell
traceShow :: P.Show a => a -> b -> b
```

```haskell
traceIO :: Print b => b -> a -> IO a
```
