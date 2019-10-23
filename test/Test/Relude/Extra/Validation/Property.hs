{-
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Test.Relude.Extra.Validation.Property
       ( validationTestList
       ) where

import Relude
import Relude.Extra.Validation

import Hedgehog (Gen, Group (..), Property, forAll, forAllWith, property, (===))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

validationTestList :: [Group]
validationTestList =
    [ validationSemigroupProps
    , validationMonoidProps
    , validationApplicativeProps
    , validationAlternativeProps
    ]

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

genFunction :: Gen (Int -> Int)
genFunction = Gen.element [(+), (*), const] <*> genSmallInt

genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear (-10) 10)

genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 3 10) Gen.unicode

asValidation :: Gen a -> Gen (Validation [Text] a)
asValidation gen = Gen.choice
    [ Success <$> gen
    , Failure <$> Gen.list (Range.linear 1 5) genSmallText
    ]

----------------------------------------------------------------------------
-- Property helpers
----------------------------------------------------------------------------

checkAssotiativityFor
    :: (Show a, Eq a) => Gen a -> (a -> a -> a) -> Property
checkAssotiativityFor gen op = property $ do
    a <- forAll gen
    b <- forAll gen
    c <- forAll gen
    a `op` (b `op` c) === (a `op` b) `op` c

----------------------------------------------------------------------------
-- Semogroup instance properties
----------------------------------------------------------------------------

validationSemigroupProps :: Group
validationSemigroupProps =
    Group "Semigroup instance for Validation property tests"
        [ ("associativity:", prop_semigroupAssociativity)
        ]

prop_semigroupAssociativity :: Property
prop_semigroupAssociativity =
    checkAssotiativityFor (asValidation genSmallText) (<>)

----------------------------------------------------------------------------
-- Monoid instance properties
----------------------------------------------------------------------------

validationMonoidProps :: Group
validationMonoidProps =
    Group "Monoid instance for Validation property tests"
        [ ("right identity:", prop_monoidRightIdentity)
        , ("left identity:", prop_monoidLeftIdentity)
        ]

prop_monoidRightIdentity :: Property
prop_monoidRightIdentity = property $ do
    x <- forAll $ asValidation genSmallText
    x <> mempty === x

prop_monoidLeftIdentity :: Property
prop_monoidLeftIdentity = property $ do
    x <- forAll $ asValidation genSmallText
    mempty <> x === x

----------------------------------------------------------------------------
-- Applicative instance properties
----------------------------------------------------------------------------

validationApplicativeProps :: Group
validationApplicativeProps =
    Group "Applicative instance for Validation property tests"
        [ ("identity:", prop_applicativeIdentity)
        , ("composition:", prop_applicativeComposition)
        , ("homomorphism:", prop_applicativeHomomorphism)
        , ("interchange:", prop_applicativeInterchange)
        , ("u *> v == (id <$ u) <*> v", prop_applicativeApplyRight)
        , ("u <* v == liftA2 const u v", prop_applicativeApplyLeft)
        ]

prop_applicativeIdentity :: Property
prop_applicativeIdentity = property $ do
    vx <- forAll $ asValidation genSmallText
    (pure id <*> vx) === vx

prop_applicativeComposition :: Property
prop_applicativeComposition = property $ do
    vf <- forAllWith (const "f") $ asValidation genFunction
    vg <- forAllWith (const "g") $ asValidation genFunction
    vx <- forAll $ asValidation genSmallInt
    (pure (.) <*> vf <*> vg <*> vx) === (vf <*> (vg <*> vx))

prop_applicativeHomomorphism :: Property
prop_applicativeHomomorphism = property $ do
    f <- forAllWith (const "f") genFunction
    x <- forAll genSmallInt
    (pure f <*> (pure x :: Validation [Text] Int)) === pure (f x)

prop_applicativeInterchange :: Property
prop_applicativeInterchange = property $ do
    vf <- forAllWith (const "f") $ asValidation genFunction
    x <- forAll genSmallInt
    (vf <*> pure x) === (pure ($ x) <*> vf)

prop_applicativeApplyRight :: Property
prop_applicativeApplyRight = property $ do
    vy <- forAll $ asValidation genSmallInt
    vx <- forAll $ asValidation genSmallInt
    (vy *> vx) === ((id <$ vy) <*> vx)

prop_applicativeApplyLeft :: Property
prop_applicativeApplyLeft = property $ do
    vy <- forAll $ asValidation genSmallInt
    vx <- forAll $ asValidation genSmallInt
    (vy <* vx) === liftA2 const vy vx

----------------------------------------------------------------------------
-- Alternative instance properties
----------------------------------------------------------------------------

validationAlternativeProps :: Group
validationAlternativeProps =
    Group "Alternative instance for Validation property tests"
        [ ("associativity:", prop_alternativeAssociativity)
        , ("right identity:", prop_alternativeRightIdentity)
        , ("left identity:", prop_alternativeLeftIdentity)
        ]

prop_alternativeAssociativity :: Property
prop_alternativeAssociativity =
    checkAssotiativityFor (asValidation genSmallText) (<|>)

prop_alternativeRightIdentity :: Property
prop_alternativeRightIdentity = property $ do
    x <- forAll $ asValidation genSmallText
    (x <|> empty) === x

prop_alternativeLeftIdentity :: Property
prop_alternativeLeftIdentity = property $ do
    x <- forAll $ asValidation genSmallText
    (empty <|> x) === x
