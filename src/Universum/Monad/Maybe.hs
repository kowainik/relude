{-# LANGUAGE Safe #-}

-- | Utility functions to work with 'Data.Maybe' data type as monad.

module Universum.Monad.Maybe
       ( (?:)
       , whenJust
       , whenJustM
       , whenNothing
       , whenNothing_
       , whenNothingM
       , whenNothingM_
       ) where

import Universum.Applicative (Applicative, pass, pure)
import Universum.Monad.Reexport (fromMaybe)
import Universum.Monad.Reexport (Maybe (..), Monad (..))

-- $setup
-- >>> import Universum.Bool (Bool (..), not)
-- >>> import Universum.Function (($))
-- >>> import Universum.Print (putTextLn, print)
-- >>> import Universum.String (readMaybe)

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

{- | Specialized version of 'for_' for 'Maybe'. It's used for code readability.
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

-- | Monadic version of 'whenJust'.
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mm f = mm >>= \m -> whenJust m f
{-# INLINE whenJustM #-}

-- | Performs default 'Applicative' action if 'Nothing' is given.
-- Otherwise returns content of 'Just' pured to 'Applicative'.
--
-- >>> whenNothing Nothing [True, False]
-- [True,False]
-- >>> whenNothing (Just True) [True, False]
-- [True]
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing  m = m
{-# INLINE whenNothing #-}

-- | Performs default 'Applicative' action if 'Nothing' is given.
-- Do nothing for 'Just'. Convenient for discarding 'Just' content.
--
-- >>> whenNothing_ Nothing $ putTextLn "Nothing!"
-- Nothing!
-- >>> whenNothing_ (Just True) $ putTextLn "Nothing!"
whenNothing_ :: Applicative f => Maybe a -> f () -> f ()
whenNothing_ Nothing m = m
whenNothing_ _       _ = pass
{-# INLINE whenNothing_ #-}

-- | Monadic version of 'whenNothing'.
whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM mm action = mm >>= \m -> whenNothing m action
{-# INLINE whenNothingM #-}

-- | Monadic version of 'whenNothingM_'.
whenNothingM_ :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM_ mm action = mm >>= \m -> whenNothing_ m action
{-# INLINE whenNothingM_ #-}
