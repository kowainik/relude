{-# LANGUAGE InstanceSigs #-}

{- |
Copyright: (c) 2014 Chris Allen, Edward Kmett
           (c) 2018 Kowainic
License:   MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Monoidal 'Validation' sibling to 'Either'.
-}

module Relude.Extra.Validation
       ( Validation(..)
       , validationToEither
       , eitherToValidation
       ) where

import Data.Function (const)
import Relude

-- | 'Validation' is 'Either' with a Left that is a 'Monoid'
data Validation e a
    = Failure e
    | Success a
    deriving (Eq, Ord, Show)

instance Functor (Validation e) where
    fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

    (<$) :: a -> Validation e b -> Validation e a
    x <$ Success _ = Success x
    _ <$ Failure e = Failure e

    {-# INLINE fmap #-}
    {-# INLINE (<$) #-}

instance Semigroup e => Applicative (Validation e) where
    pure :: a -> Validation e a
    pure = Success

    liftA2 :: (a -> b -> c) -> Validation e a -> Validation e b -> Validation e c
    liftA2 _ (Failure e) (Failure e') = Failure (e <> e')
    liftA2 _ (Failure e) (Success _) = Failure e
    liftA2 _ (Success _) (Failure e) = Failure e
    liftA2 f (Success a) (Success a') = Success (f a a')

    (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
    Failure e <*> b = Failure $ case b of
                                  Failure e' -> e <> e'
                                  Success _  -> e
    Success _ <*> Failure e  = Failure e
    Success f <*> Success a = Success (f a)


    (*>) :: Validation e a -> Validation e b -> Validation e b
    (*>) = liftA2 (flip const)

    (<*) :: Validation e a -> Validation e b -> Validation e a
    (<*) = liftA2 const

    {-# INLINE pure #-}
    {-# INLINE liftA2 #-}
    {-# INLINE (<*>) #-}
    {-# INLINE (*>) #-}
    {-# INLINE (<*) #-}

instance (Semigroup e, Monoid e) => Alternative (Validation e) where
    empty :: Validation e a
    empty = Failure mempty

    (<|>) :: Validation e a -> Validation e a -> Validation e a
    s@Success{} <|> _ = s
    _ <|> s@Success{} = s
    Failure e <|> Failure e' = Failure (e <> e')

    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance Foldable (Validation e) where
    fold :: Monoid m => Validation e m -> m
    fold (Success a) = a
    fold (Failure _) = mempty

    foldMap :: Monoid m => (a -> m) -> Validation e a -> m
    foldMap f = foldr (mappend . f) mempty

    foldr :: (a -> b -> b) -> b -> Validation e a -> b
    foldr f x (Success a) = f a x
    foldr _ x (Failure _) = x

    {-# INLINE fold #-}
    {-# INLINE foldMap #-}
    {-# INLINE foldr #-}

instance Traversable (Validation e) where
    traverse :: Applicative f => (a -> f b) -> Validation e a -> f (Validation e b)
    traverse f (Success a) = Success <$> f a
    traverse _ (Failure e) = pure (Failure e)

    {-# INLINE traverse #-}

instance Bifunctor Validation where
    bimap :: (e -> d) -> (a -> b) -> Validation e a -> Validation d b
    bimap f _ (Failure e) = Failure (f e)
    bimap _ g (Success a) = Success (g a)

    first :: (e -> d) -> Validation e a -> Validation d a
    first f (Failure e) = Failure (f e)
    first _ (Success a) = Success a

    second :: (a -> b) -> Validation e a -> Validation e b
    second _ (Failure e) = Failure e
    second g (Success a) = Success (g a)

    {-# INLINE bimap #-}
    {-# INLINE first #-}
    {-# INLINE second #-}

instance Bifoldable Validation where
    bifoldMap :: Monoid m => (e -> m) -> (a -> m) -> Validation e a -> m
    bifoldMap f _ (Failure e) = f e
    bifoldMap _ g (Success a) = g a

    {-# INLINE bifoldMap #-}

instance Bitraversable Validation where
    bitraverse :: Applicative f
               => (e -> f d) -> (a -> f b) -> Validation e a -> f (Validation d b)
    bitraverse f _ (Failure e) = Failure <$> f e
    bitraverse _ g (Success a) = Success <$> g a

    {-# INLINE bitraverse #-}

-- | Transform a 'Validation' into an 'Either'.
validationToEither :: Validation e a -> Either e a
validationToEither x = case x of
                         Failure e -> Left e
                         Success a -> Right a

{-# INLINE validationToEither #-}

-- | Transform an 'Either' into a 'Validation'.
eitherToValidation :: Either e a -> Validation e a
eitherToValidation x = case x of
                         Left e  -> Failure e
                         Right a -> Success a

{-# INLINE eitherToValidation #-}
