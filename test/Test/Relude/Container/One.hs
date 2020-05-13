module Test.Relude.Container.One
    ( oneProps
    ) where

import Relude

import Hedgehog (Gen, Group (..), Property, forAll, property, (===))

import Test.Relude.Gen (genInt, genUtf8Text)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


oneProps :: Group
oneProps = Group "'One' typeclass property tests"
    [ ( "length (one @[a] x)             ≡ 1", oneListProp)
    , ( "length (one @(NonEmpty a) x)    ≡ 1", oneNonEmptyProp)
    , ( "length (one @(Seq a) x)         ≡ 1", oneSeqProp)
    , ( "length (one @Text x)            ≡ 1", oneTextProp)
    , ( "length (one @LText x)           ≡ 1", oneLTextProp)
    , ( "length (one @ByteString x)      ≡ 1", oneByteStringProp)
    , ( "length (one @LByteString x)     ≡ 1", oneLByteStringProp)
    , ( "length (one @ShortByteString x) ≡ 1", oneShortByteStringProp)
    , ( "length (one @(Map k v) x)       ≡ 1", oneMapProp)
    , ( "length (one @(HashMap k v) x)   ≡ 1", oneHashMapProp)
    , ( "length (one @(IntMap v) x)      ≡ 1", oneIntMapProp)
    , ( "length (one @(Set a) x)         ≡ 1", oneSetProp)
    , ( "length (one @(HashSet a) x)     ≡ 1", oneHashSetProp)
    , ( "length (one @(IntSet x)         ≡ 1", oneIntSetProp)
    ]

oneListProp :: Property
oneListProp = property $ do
    x <- forAll genInt
    length (one @[Int] x) === 1

oneNonEmptyProp :: Property
oneNonEmptyProp = property $ do
    x <- forAll genInt
    length (one @(NonEmpty Int) x) === 1

oneSeqProp :: Property
oneSeqProp = property $ do
    x <- forAll genInt
    length (one @(Seq Int) x) === 1

oneTextProp :: Property
oneTextProp = property $ do
    x <- forAll Gen.unicode
    T.length (one @Text x) === 1

oneLTextProp :: Property
oneLTextProp = property $ do
    x <- forAll Gen.unicode
    TL.length (one @LText x) === 1

oneByteStringProp :: Property
oneByteStringProp = property $ do
    x <- forAll genWord8
    BS.length (one @ByteString x) === 1

oneLByteStringProp :: Property
oneLByteStringProp = property $ do
    x <- forAll genWord8
    LBS.length (one @LByteString x) === 1

oneShortByteStringProp :: Property
oneShortByteStringProp = property $ do
    x <- forAll genWord8
    SBS.length (one @ShortByteString x) === 1

oneMapProp :: Property
oneMapProp = property $ do
    k <- forAll genUtf8Text
    v <- forAll genInt
    length (one @(Map Text Int) (k, v)) === 1

oneHashMapProp :: Property
oneHashMapProp = property $ do
    k <- forAll genUtf8Text
    v <- forAll genInt
    length (one @(HashMap Text Int) (k, v)) === 1

oneIntMapProp :: Property
oneIntMapProp = property $ do
    k <- forAll genInt
    v <- forAll genUtf8Text
    length (one @(IntMap Text) (k, v)) === 1

oneSetProp :: Property
oneSetProp = property $ do
    v <- forAll genUtf8Text
    length (one @(Set Text) v) === 1

oneHashSetProp :: Property
oneHashSetProp = property $ do
    v <- forAll genUtf8Text
    length (one @(HashSet Text) v) === 1

oneIntSetProp :: Property
oneIntSetProp = property $ do
    v <- forAll genInt
    IntSet.size (one v) === 1


genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded
