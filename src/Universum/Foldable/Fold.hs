{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Fixes and additions to 'Foldable'.

module Universum.Foldable.Fold
       ( flipfoldl'
       , safeHead
       , sum
       , product

#if __GLASGOW_HASKELL__ >= 800
       , elem
       , notElem
#endif

       , allM
       , anyM
       , andM
       , orM
       ) where

import Universum.Applicative (pure)
import Universum.Base (IO, Num (..))
import Universum.Bool (Bool (..))
import Universum.Container.Reexport (HashSet, Set)
import Universum.Foldable.Reexport (Foldable (..))
import Universum.Function (flip, (.))
import Universum.Monad.Reexport (Maybe (..), Monad (..))

import qualified Data.Foldable as F

#if ( __GLASGOW_HASKELL__ >= 800 )
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Universum.Base (Constraint, Eq, Type)
#endif

-- $setup
-- :set -XOverloadedStrings
-- >>> import Universum.Base (String, Rational, even, (/))
-- >>> import Universum.Bool (when)
-- >>> import Universum.Monad (Maybe (..), (>=>))
-- >>> import Universum.Print (print, putTextLn)
-- >>> import Universum.String (Text, readMaybe)
-- >>> import qualified Data.HashMap.Strict as HashMap

safeHead :: Foldable f => f a -> Maybe a
safeHead = foldr (\x _ -> Just x) Nothing
{-# INLINE safeHead #-}

{- | Similar to 'foldl'' but takes a function with its arguments flipped.

>>> flipfoldl' (/) 5 [2,3] :: Rational
15 % 2

-}
flipfoldl' :: Foldable f => (a -> b -> b) -> b -> f a -> b
flipfoldl' f = foldl' (flip f)
{-# INLINE flipfoldl' #-}

{- | Stricter version of 'F.sum'.

>>> sum [1..10]
55
-}
sum :: forall a f . (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0
{-# INLINE sum #-}

{- | Stricter version of 'F.product'.

>>> product [1..10]
3628800
-}
product :: forall a f . (Foldable f, Num a) => f a -> a
product = foldl' (*) 1
{-# INLINE product #-}

#if __GLASGOW_HASKELL__ >= 800
{- | Like 'F.elem' but doesn't work on 'Set' and 'HashSet' for performance reasons.

>>> elem 'x' ("abc" :: String)
False
>>> elem False (one True :: Set Bool)
...
    • Do not use 'elem' and 'notElem' methods from 'Foldable' on Set
      Suggestions:
          Instead of
              elem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              member :: ??? -- TODO
...
          Instead of
              notElem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              notMember :: ??? -- TODO
...
-}
elem :: (Foldable f, DisallowElem f, Eq a) => a -> f a -> Bool
elem = F.elem
{-# INLINE elem #-}

{- | Like 'F.notElem' but doesn't work on 'Set' and 'HashSet' for performance reasons.

>>> notElem 'x' ("abc" :: String)
True
>>> notElem False (one True :: Set Bool)
...
    • Do not use 'elem' and 'notElem' methods from 'Foldable' on Set
      Suggestions:
          Instead of
              elem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              member :: ??? -- TODO
...
          Instead of
              notElem :: (Foldable t, Eq a) => a -> t a -> Bool
          use
              notMember :: ??? -- TODO
...
-}
notElem :: (Foldable f, DisallowElem f, Eq a) => a -> f a -> Bool
notElem = F.notElem
{-# INLINE notElem #-}
#endif

{- | Monadic version of 'F.and'.

>>> andM [Just True, Just False]
Just False
>>> andM [Just True]
Just True
>>> andM [Just True, Just False, Nothing]
Just False
>>> andM [Just True, Nothing]
Nothing
>>> andM [putTextLn "1" >> pure True, putTextLn "2" >> pure False, putTextLn "3" >> pure True]
1
2
False
-}
andM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
andM = go . toList
  where
    go []     = pure True
    go (p:ps) = do
        q <- p
        if q then go ps else pure False

{- | Monadic version of 'F.or'.

>>> orM [Just True, Just False]
Just True
>>> orM [Just True, Nothing]
Just True
>>> orM [Nothing, Just True]
Nothing
-}
orM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
orM = go . toList
  where
    go []     = pure False
    go (p:ps) = do
        q <- p
        if q then pure True else go ps

{- | Monadic version of 'F.all'.

>>> allM (readMaybe >=> pure . even) ["6", "10"]
Just True
>>> allM (readMaybe >=> pure . even) ["5", "aba"]
Just False
>>> allM (readMaybe >=> pure . even) ["aba", "10"]
Nothing
-}
allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = go . toList
  where
    go []     = pure True
    go (x:xs) = do
        q <- p x
        if q then go xs else pure False

{- | Monadic  version of 'F.any'.

>>> anyM (readMaybe >=> pure . even) ["5", "10"]
Just True
>>> anyM (readMaybe >=> pure . even) ["10", "aba"]
Just True
>>> anyM (readMaybe >=> pure . even) ["aba", "10"]
Nothing
-}
anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = go . toList
  where
    go []     = pure False
    go (x:xs) = do
        q <- p x
        if q then pure True else go xs

{-# SPECIALIZE andM :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE orM  :: [IO Bool] -> IO Bool #-}
{-# SPECIALIZE anyM :: (a -> IO Bool) -> [a] -> IO Bool #-}
{-# SPECIALIZE allM :: (a -> IO Bool) -> [a] -> IO Bool #-}

----------------------------------------------------------------------------
-- Type level tricks
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 800
type family DisallowElem (f :: Type -> Type) :: Constraint where
    DisallowElem     Set = TypeError (ElemErrorMessage Set)
    DisallowElem HashSet = TypeError (ElemErrorMessage HashSet)
    DisallowElem f       = ()

type family ElemErrorMessage (t :: k) :: ErrorMessage where
    ElemErrorMessage t =
             Text "Do not use 'elem' and 'notElem' methods from 'Foldable' on " :<>: ShowType t
        :$$: Text "Suggestions:"
        :$$: Text "    Instead of"
        :$$: Text "        elem :: (Foldable t, Eq a) => a -> t a -> Bool"
        :$$: Text "    use"
        :$$: Text "        member :: ??? -- TODO"
        :$$: Text ""
        :$$: Text "    Instead of"
        :$$: Text "        notElem :: (Foldable t, Eq a) => a -> t a -> Bool"
        :$$: Text "    use"
        :$$: Text "        notMember :: ??? -- TODO"
        :$$: Text ""
#endif
