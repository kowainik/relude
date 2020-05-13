module Test.Relude.Gen
    ( genInt
      -- * Strings
    , genUtf8String
    , genUtf8Text
    , genUtf8ByteString
      -- * Lists
    , genIntList
    , genBoolList
    ) where

import Relude

import Hedgehog (Gen)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genInt :: Gen Int
genInt = Gen.enumBounded

genUtf8String :: Gen String
genUtf8String = Gen.string (Range.linear 0 1000) Gen.unicode

genUtf8Text :: Gen Text
genUtf8Text = Gen.text (Range.linear 0 1000) Gen.unicode

genUtf8ByteString :: Gen ByteString
genUtf8ByteString = Gen.utf8 (Range.linear 0 1000) Gen.unicode

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 1000) Gen.enumBounded

genBoolList :: Gen [Bool]
genBoolList = Gen.list (Range.linear 0 1000) Gen.bool
