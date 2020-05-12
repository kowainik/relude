{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Monadic boolean combinators.
-}

module Relude.Bool.Guard
    ( guarded
    , guardM
    , ifM
    , unlessM
    , whenM
    , (&&^)
    , (||^)
    ) where

import Relude.Applicative (Alternative, Applicative (..), empty)
import Relude.Bool.Reexport (Bool (..), guard, unless, when)
import Relude.Function (flip)
import Relude.Monad (Monad, MonadPlus, (>>=))


-- $setup
-- >>> import Relude.Applicative (pure)
-- >>> import Relude.Bool.Reexport (Bool (..))
-- >>> import Relude.Debug (error)
-- >>> import Relude.Function (($))
-- >>> import Relude.Monad (Maybe (..))
-- >>> import Relude.Print (putTextLn)
-- >>> import Relude (Int, String, even, const)

{- | Monadic version of 'when'.

>>> whenM (pure False) $ putTextLn "No text :("
>>> whenM (pure True)  $ putTextLn "Yes text :)"
Yes text :)
>>> whenM (Just True) (pure ())
Just ()
>>> whenM (Just False) (pure ())
Just ()
>>> whenM Nothing (pure ())
Nothing
-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
{-# INLINE whenM #-}

{- | Monadic version of 'unless'.

>>> unlessM (pure False) $ putTextLn "No text :("
No text :(
>>> unlessM (pure True) $ putTextLn "Yes text :)"
-}
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= flip unless m
{-# INLINE unlessM #-}

{- | Monadic version of @if-then-else@.

>>> ifM (pure True) (putTextLn "True text") (putTextLn "False text")
True text
-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
{-# INLINE ifM #-}

{- | Monadic version of 'guard'. Occasionally useful.
Here some complex but real-life example:

@
findSomePath :: IO (Maybe FilePath)

somePath :: MaybeT IO FilePath
somePath = do
    path <- MaybeT findSomePath
    guardM $ liftIO $ doesDirectoryExist path
    return path
@
-}
guardM :: MonadPlus m => m Bool -> m ()
guardM f = f >>= guard
{-# INLINE guardM #-}

{- | Either lifts a value into an alternative context or gives a
minimal value depending on a predicate.

>>> guarded even 3 :: [Int]
[]
>>> guarded even 2 :: [Int]
[2]
>>> guarded (const True) "hello" :: Maybe String
Just "hello"
>>> guarded (const False) "world" :: Maybe String
Nothing

You can use this function to implement smart constructors simpler:

@
__newtype__ HttpHost = HttpHost
    { unHttpHost :: Text
    }

mkHttpHost :: Text -> Maybe HttpHost
mkHttpHost host = HttpHost \<$\> 'guarded' (not . Text.null) host
@

@since 0.6.0.0
-}
guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty
{-# INLINE guarded #-}

{- | Monadic version of 'Data.Bool.(&&)' operator.

>>> Just False &&^ Just True
Just False
>>> Just True &&^ Just True
Just True
>>> Just True &&^ Nothing
Nothing
>>> Just False &&^ Nothing
Just False
>>> Just False &&^ error "Shouldn't be evaluated"
Just False
-}
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) e1 e2 = ifM e1 e2 (pure False)
{-# INLINE (&&^) #-}

{- | Monadic version of 'Data.Bool.(||)' operator.

>>> Just False ||^ Just True
Just True
>>> Just False ||^ Just False
Just False
>>> Just False ||^ Nothing
Nothing
>>> Just True ||^ Nothing
Just True
>>> Just True ||^ error "Shouldn't be evaluated"
Just True
-}
(||^) :: Monad m => m Bool -> m Bool -> m Bool
e1 ||^ e2 = ifM e1 (pure True) e2
{-# INLINE (||^) #-}
