Exceptions
==========

MonadError
----------

```haskell
class Monad m => MonadError e (m :: * -> *) | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a
```

#### Except

```haskell
type Except e = ExceptT e Identity
```

*Example*:

```haskell
```

#### ExceptT

```haskell
newtype ExceptT e (m :: * -> *) a
  = Control.Monad.Trans.Except.ExceptT (m (Either e a))
```

*Example*:

```haskell
```

#### throwError

```haskell
throwError :: MonadError e m => e -> m a
```

#### catchError

```haskell
catchError :: MonadError e m => m a -> (e -> m a) -> m a
```

#### runExcept

```haskell
runExcept :: Except e a -> Either e a
```

#### runExceptT

```haskell
runExceptT :: ExceptT e m a -> m (Either e a)
```

Exceptions
----------

```haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  GHC.Exception.displayException :: e -> String
```

#### throwIO

```haskell
throwIO :: (MonadIO m, Exception e) => e -> m a
```

#### throwSTM

```haskell
throwSTM :: Exception e => e -> STM a
```

#### throwTo

```haskell
throwTo :: (MonadIO m, Exception e) => ThreadId -> e -> m ()
```

Utilities
---------

#### hush

```haskell
hush :: Alternative m => Either e a -> m a
```

#### note

```haskell
note :: (MonadError e m, Applicative m) => e -> Maybe a -> m a
```

#### tryIO

```haskell
tryIO :: MonadIO m => IO a -> ExceptT IOException m a
```

Fatal Errors
------------

```haskell
data FatalError = FatalError {fatalErrorMessage :: Text}
```

#### panic

```haskell
panic :: Text -> a
```

Terminate with an uncatchable fatal error.

```haskell
> panic "Fatal error occured. 
```
