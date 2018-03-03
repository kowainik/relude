{-# LANGUAGE Safe #-}

-- | This module contains useful functions to work with 'Functor' type class.

module Universum.Functor.Fmap
       ( map
       , (<<$>>)
       ) where

import Universum.Function ((.))
import Universum.Functor.Reexport (Functor (..))

-- $setup
-- >>> import Universum.Base (negate)
-- >>> import Universum.Bool (Bool (..), not)
-- >>> import Universum.Lifted (getLine)
-- >>> import Universum.Monad (Maybe (..))
-- >>> import Universum.String (toString)

{- | 'Prelude.map' generalized to 'Functor'.

>>> map not (Just True)
Just False
>>> map not [True,False,True,True]
[False,True,False,False]

-}
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- $setup
-- >>> import Universum.Base (negate)
-- >>> import Universum.Monad (Maybe (..))

-- | Alias for @fmap . fmap@. Convenient to work with two nested 'Functor's.
--
-- >>> negate <<$>> Just [1,2,3]
-- Just [-1,-2,-3]
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>
