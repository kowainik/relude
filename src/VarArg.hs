{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module VarArg
    ( (...)
    ) where

-- | Allows to apply function to result of another function with multiple
-- arguments
--
-- >>> (show ... (+)) 1 2
-- "3"
-- >>> show ... 5
-- "5"
-- (null ... zip5) [1] [2] [3] [] [5]
-- True
--
-- Inspired by <http://stackoverflow.com/questions/9656797/variadic-compose-function>
class Composition a b c | a b -> c where
    (...) :: a -> b -> c

instance (a ~ c, r ~ b) =>
         Composition (a -> b) c r where
    f ... g = f g
    {-# INLINE (...) #-}

instance (Composition (a -> b) d r1, r ~ (c -> r1)) =>
         Composition (a -> b) (c -> d) r where
    (f ... g) c = f ... g c
    {-# INLINE (...) #-}
