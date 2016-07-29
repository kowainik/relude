Exception Handling
==================

#### MonadError

```haskell
class Monad m => MonadError e (m :: * -> *) | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a
```

```haskell
type Except e = ExceptT e Identity
```


```haskell
newtype ExceptT e (m :: * -> *) a
  = Control.Monad.Trans.Except.ExceptT (m (Either e a))
```


```haskell
throwError :: MonadError e m => e -> m a
```

```haskell
catchError :: MonadError e m => m a -> (e -> m a) -> m a
```

```haskell
runExcept :: Except e a -> Either e a
```

```haskell
runExceptT :: ExceptT e m a -> m (Either e a)
```

#### Exceptions

```haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  GHC.Exception.displayException :: e -> String
```

```haskell
throwIO :: (MonadIO m, Exception e) => e -> m a
```

```haskell
throwTo :: (MonadIO m, Exception e) => ThreadId -> e -> m ()
```

```haskell
throwSTM :: Exception e => e -> STM a
```

```haskell
throwError :: MonadError e m => e -> m a
```

#### Panic

```haskell
data FatalError = FatalError {msg :: Text}
```

```haskell
panic :: Text -> a
```
