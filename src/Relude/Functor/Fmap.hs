{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

-- | This module contains useful functions to work with 'Functor' type class.

module Relude.Functor.Fmap
       ( (<<$>>)
       , (<&>)
       ) where

import Relude.Function ((.))
import Relude.Functor.Reexport (Functor (..))

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#else
import Data.Functor ((<$>))
-- | Flipped version of '<$>'.
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
#endif

-- $setup
-- >>> import Relude.Base (negate)
-- >>> import Relude.Monad (Maybe (..))

-- | Alias for @fmap . fmap@. Convenient to work with two nested 'Functor's.
--
-- >>> negate <<$>> Just [1,2,3]
-- Just [-1,-2,-3]
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>
