{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

{- |
Copyright:  (c) 2014 Chris Allen, Edward Kmett
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

'Validation' is a monoidal sibling to 'Either'. Use 'Validation' on operations that
might fail with multiple errors that you want to preserve.
-}


module Relude.Extra.Validation
       (
       -- * How to use
       -- $use
         Validation(..)
       , validationToEither
       , eitherToValidation
       ) where

import Relude


{- | $setup
>>> :set -XTypeApplications -XOverloadedStrings
-}

{- $use

Take for example a type @Computer@ that needs to be validated:

>>> data Computer = Computer { ram :: Int, cpus :: Int } deriving (Eq, Show)

You can validate that the computer has a minimum of 16 GB of RAM:

>>> :{
validateRam :: Int -> Validation [Text] Int
validateRam ram
 | ram >= 16 = Success ram
 | otherwise = Failure ["Not enough RAM"]
:}

and that the processor has at least two CPUs:

>>> :{
validateCpus :: Int -> Validation [Text] Int
validateCpus cpus
 | cpus >= 2 = Success cpus
 | otherwise = Failure ["Not enough CPUs"]
:}

You can use these functions with the 'Applicative' instance of the 'Validation'
type to construct a validated @Computer@. You will get either (pun intended) a
valid @Computer@ or the errors that prevent it from being considered valid.

Like so:

>>> :{
mkComputer :: Int -> Int -> Validation [Text] Computer
mkComputer ram cpus = Computer <$> validateRam ram <*> validateCpus cpus
:}

Using @mkComputer@ we get a @Success Computer@ or a list with all possible errors:

>>> mkComputer 16 2
Success (Computer {ram = 16, cpus = 2})

>>> mkComputer 16 1
Failure ["Not enough CPUs"]

>>> mkComputer 15 2
Failure ["Not enough RAM"]

>>> mkComputer 15 1
Failure ["Not enough RAM","Not enough CPUs"]
-}

-- | 'Validation' is 'Either' with a Left that is a 'Monoid'
data Validation e a
    = Failure e
    | Success a
    deriving (Eq, Ord, Show)

instance Functor (Validation e) where
    fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)
    {-# INLINE fmap #-}

    (<$) :: a -> Validation e b -> Validation e a
    x <$ Success _ = Success x
    _ <$ Failure e = Failure e
    {-# INLINE (<$) #-}

{- | __Examples__
>>> let a = Success "First success." :: Validation [Text] Text
>>> let b = Success " Second success." :: Validation [Text] Text
>>> let c = Failure ["Not correct"] :: Validation [Text] Text

>>> a <> b
Success "First success. Second success."

>> a <> c
Failure ["Not correct"]
-}

instance (Semigroup e, Semigroup a) => Semigroup (Validation e a) where
    (<>) :: Validation e a -> Validation e a -> Validation e a
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance (Semigroup e, Monoid a) => Monoid (Validation e a) where
    mempty :: Validation e a
    mempty = Success mempty
    {-# INLINE mempty #-}

{- | __Examples__

>>> let fa = Success (*3) :: Validation [Text] (Int -> Int)
>>> let ga = Success (*4) :: Validation [Text] (Int -> Int)
>>> let a = Success 1 :: Validation [Text] Int
>>> let b = Success 7 :: Validation [Text] Int
>>> let c = Failure ["Not correct"] :: Validation [Text] Int
>>> let d = Failure ["Not correct either"] :: Validation [Text] Int

>>> fa <*> b
Success 21

>>> fa <*> c
Failure ["Not correct"]

>>> c *> d *> b
Failure ["Not correct","Not correct either"]

>>> liftA2 (+) a b
Success 8

>>> liftA2 (+) a c
Failure ["Not correct"]
-}
instance Semigroup e => Applicative (Validation e) where
    pure :: a -> Validation e a
    pure = Success
    {-# INLINE pure #-}

    (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
    Failure e <*> b = Failure $ case b of
                                  Failure e' -> e <> e'
                                  Success _  -> e
    Success _ <*> Failure e  = Failure e
    Success f <*> Success a = Success (f a)
    {-# INLINE (<*>) #-}

    (*>) :: Validation e a -> Validation e b -> Validation e b
    Failure el *> Failure er = Failure (el <> er)
    Failure e  *> Success _  = Failure e
    Success _  *> Failure e  = Failure e
    Success _  *> Success b  = Success b
    {-# INLINE (*>) #-}

    (<*) :: Validation e a -> Validation e b -> Validation e a
    Failure el <* Failure er = Failure (el <> er)
    Failure e  <* Success _  = Failure e
    Success _  <* Failure e  = Failure e
    Success a  <* Success _  = Success a
    {-# INLINE (<*) #-}

instance (Semigroup e, Monoid e) => Alternative (Validation e) where
    empty :: Validation e a
    empty = Failure mempty
    {-# INLINE empty #-}

    (<|>) :: Validation e a -> Validation e a -> Validation e a
    s@Success{} <|> _ = s
    _ <|> s@Success{} = s
    Failure e <|> Failure e' = Failure (e <> e')
    {-# INLINE (<|>) #-}

instance Foldable (Validation e) where
    fold :: Monoid m => Validation e m -> m
    fold (Success a) = a
    fold (Failure _) = mempty
    {-# INLINE fold #-}

    foldMap :: Monoid m => (a -> m) -> Validation e a -> m
    foldMap _ (Failure _) = mempty
    foldMap f (Success a) = f a
    {-# INLINE foldMap #-}

    foldr :: (a -> b -> b) -> b -> Validation e a -> b
    foldr f x (Success a) = f a x
    foldr _ x (Failure _) = x
    {-# INLINE foldr #-}

instance Traversable (Validation e) where
    traverse :: Applicative f => (a -> f b) -> Validation e a -> f (Validation e b)
    traverse f (Success a) = Success <$> f a
    traverse _ (Failure e) = pure (Failure e)
    {-# INLINE traverse #-}

    sequenceA :: Applicative f => Validation e (f a) -> f (Validation e a)
    sequenceA = traverse id
    {-# INLINE sequenceA #-}

instance Bifunctor Validation where
    bimap :: (e -> d) -> (a -> b) -> Validation e a -> Validation d b
    bimap f _ (Failure e) = Failure (f e)
    bimap _ g (Success a) = Success (g a)
    {-# INLINE bimap #-}

    first :: (e -> d) -> Validation e a -> Validation d a
    first f (Failure e) = Failure (f e)
    first _ (Success a) = Success a
    {-# INLINE first #-}

    second :: (a -> b) -> Validation e a -> Validation e b
    second _ (Failure e) = Failure e
    second g (Success a) = Success (g a)
    {-# INLINE second #-}

#if MIN_VERSION_base(4,10,0)
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
#endif

{- | Transform a 'Validation' into an 'Either'.

>>> validationToEither (Success "whoop")
Right "whoop"

>>> validationToEither (Failure "nahh")
Left "nahh"

-}
validationToEither :: Validation e a -> Either e a
validationToEither = \case
    Failure e -> Left e
    Success a -> Right a
{-# INLINE validationToEither #-}

{- | Transform an 'Either' into a 'Validation'.

>>> eitherToValidation (Right "whoop")
Success "whoop"

>>> eitherToValidation (Left "nahh")
Failure "nahh"

-}
eitherToValidation :: Either e a -> Validation e a
eitherToValidation = \case
    Left e  -> Failure e
    Right a -> Success a
{-# INLINE eitherToValidation #-}
