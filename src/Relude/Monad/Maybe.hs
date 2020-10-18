{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Utility functions to work with 'Relude.Maybe' data type as monad.
-}

module Relude.Monad.Maybe
    ( -- * Combinators
      (?:)
    , whenJust
    , whenJust'
    , whenJustM
    , whenJustM'
    , whenNothing
    , whenNothing'
    , whenNothing_
    , whenNothing_'
      -- * Monadic combinators
    , whenNothingM
    , whenNothingM'
    , whenNothingM_
    , whenNothingM_'
    , mapMaybeM
    ) where

import Relude.Applicative (Applicative, pass, pure)
import Relude.Foldable.Reexport (mapM)
import Relude.Function ((.), const, flip)
import Relude.Functor.Reexport (fmap)
import Relude.Monad.Reexport ((=<<), Maybe (..), Monad (..), catMaybes, fromMaybe, maybe)


-- $setup
-- >>> import Relude

{- | Similar to 'fromMaybe' but with flipped arguments.

>>> readMaybe "True" ?: False
True

>>> readMaybe "Tru" ?: False
False
-}
infixr 0 ?:
(?:) :: Maybe a -> a -> a
mA ?: b = fromMaybe b mA
{-# INLINE (?:) #-}

{- | Specialized version of 'Relude.for_' for 'Maybe'. It's used for code readability.

Also helps to avoid space leaks:
<http://www.snoyman.com/blog/2017/01/foldable-mapm-maybe-and-recursive-functions Foldable.mapM_ space leak>.

>>> whenJust Nothing $ \b -> print (not b)
>>> whenJust (Just True) $ \b -> print (not b)
False
-}
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pass
{-# INLINE whenJust #-}

{- | Flipped version of `whenJust`
-}
whenJust' :: Applicative f => (a -> f ()) -> Maybe a -> f ()
whenJust' = maybe pass
{-# INLINE whenJust' #-}

{- | Monadic version of 'whenJust'.

>>> whenJustM (pure Nothing) $ \b -> print (not b)
>>> whenJustM (pure $ Just True) $ \b -> print (not b)
False
-}
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mm f = mm >>= \m -> whenJust m f
{-# INLINE whenJustM #-}

{- | Flipped version of `whenJustM`
-}
whenJustM' :: Monad m => (a -> m ()) -> m (Maybe a) -> m ()
whenJustM' = (=<<) . whenJust'
{-# INLINE whenJustM' #-}

{- | Performs default 'Applicative' action if 'Nothing' is given.
Otherwise returns content of 'Just' pured to 'Applicative'.

>>> whenNothing Nothing [True, False]
[True,False]
>>> whenNothing (Just True) [True, False]
[True]
-}
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing  m = m
{-# INLINE whenNothing #-}

{- | Flipped version of `whenNothing`
-}
whenNothing' :: Applicative f => f a -> Maybe a -> f a
whenNothing' = flip maybe pure
{-# INLINE whenNothing' #-}

{- | Performs default 'Applicative' action if 'Nothing' is given.
Do nothing for 'Just'. Convenient for discarding 'Just' content.

>>> whenNothing_ Nothing $ putTextLn "Nothing!"
Nothing!
>>> whenNothing_ (Just True) $ putTextLn "Nothing!"
-}
whenNothing_ :: Applicative f => Maybe a -> f () -> f ()
whenNothing_ Nothing m = m
whenNothing_ _       _ = pass
{-# INLINE whenNothing_ #-}

{- | Flipped version of `whenNothing_`
-}
whenNothing_' :: Applicative f => f () -> Maybe a -> f ()
whenNothing_' = flip maybe (const pass)
{-# INLINE whenNothing_' #-}

{- | Monadic version of 'whenNothingM'.

>>> whenNothingM (pure $ Just True) $ True <$ putTextLn "Is Just!"
True
>>> whenNothingM (pure Nothing) $ False <$ putTextLn "Is Nothing!"
Is Nothing!
False
-}
whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM mm action = mm >>= \m -> whenNothing m action
{-# INLINE whenNothingM #-}

{- | Flipped version of `whenNothingM`
-}
whenNothingM' :: Monad m => m a -> m (Maybe a) -> m a
whenNothingM' = (=<<) . whenNothing'
{-# INLINE whenNothingM' #-}

{- | Monadic version of 'whenNothingM_'.

>>> whenNothingM_ (pure $ Just True) $ putTextLn "Is Just!"
>>> whenNothingM_ (pure Nothing) $ putTextLn "Is Nothing!"
Is Nothing!
-}
whenNothingM_ :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM_ mm action = mm >>= \m -> whenNothing_ m action
{-# INLINE whenNothingM_ #-}

{- | Flipped version of `whenNothingM_`
-}
whenNothingM_' :: Monad m => m () -> m (Maybe a) -> m ()
whenNothingM_' = (=<<) . whenNothing_'
{-# INLINE whenNothingM_' #-}

{- | The monadic version of the 'Data.Maybe.mapMaybe' function.

>>> :{
evenInHalf :: Int -> IO (Maybe Int)
evenInHalf n
    | even n = pure $ Just $ n `div` 2
    | otherwise = pure Nothing
:}

>>> mapMaybeM evenInHalf [1..10]
[1,2,3,4,5]

@since 0.6.0.0
-}
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f
{-# INLINE mapMaybeM #-}
