{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Convenient utils to work with 'Applicative'. There were more functions in this module
(see <https://www.stackage.org/haddock/lts-8.9/protolude-0.1.10/Applicative.html protolude version>)
but only convenient ans most used are left.
-}

module Relude.Applicative
       ( module Control.Applicative
       , pass
       , appliedTo
       ) where

import Control.Applicative (Alternative (..), Applicative (..), Const (..), ZipList (..), liftA2,
                            liftA3, optional, (<**>))

-- $setup
-- >>> import Relude
-- >>> import Relude.Monad (Maybe)

-- | Shorter alias for @pure ()@.
--
-- >>> pass :: Maybe ()
-- Just ()
pass :: Applicative f => f ()
pass = pure ()
{-# INLINE pass #-}

{- | Named version of the '<**>' operator, which is '<*>' but flipped. For
chaining applicative operations in forward applications using
'Relude.Function.&'.

>>> Just (+ 1) & appliedTo (Just 2)
Just 3
>>> Just (+) & appliedTo (Just 1) & appliedTo (Just 2)
Just 3
>>> Nothing & appliedTo (Just 2)
Nothing
-}
appliedTo :: Applicative f => f a -> f (a -> b) -> f b
appliedTo = (<**>)
{-# INLINE appliedTo #-}
