module Test.Relude.Property
       ( hedgehogTestList
       ) where

import Relude

import Data.List (nub)
import Hedgehog (Group (..), Property, assert, forAll, property, (===))

import Test.Relude.Container.One (oneProps)
import Test.Relude.Extra.Validation.Property (validationLaws)
import Test.Relude.Gen (genBoolList, genIntList, genUtf8ByteString, genUtf8String, genUtf8Text)


hedgehogTestList :: [Group]
hedgehogTestList =
    [ utfProps
    , listProps
    , logicProps
    , oneProps
    ] <> validationLaws

----------------------------------------------------------------------------
-- utf8 conversion
----------------------------------------------------------------------------

utfProps :: Group
utfProps = Group "utf8 conversion property tests"
    [ ("String to ByteString invertible:", prop_StringToBytes)
    , ("Text to ByteString invertible:", prop_TextToBytes)
    , ("ByteString to Text or String invertible:" , prop_BytesTo)
    ]

-- "\65534" fails, but this is from BU.toString
-- > import qualified Data.ByteString.UTF8 as BU
-- > BU.toString (BU.fromString "\65534") == "\65533"
-- > True
prop_StringToBytes :: Property
prop_StringToBytes = property $ do
    str <- forAll genUtf8String
    assert $ str == decodeUtf8 @_ @ByteString  (encodeUtf8 str)
          && str == decodeUtf8 @_ @LByteString (encodeUtf8 str)


prop_TextToBytes :: Property
prop_TextToBytes = property $ do
    txt <- forAll genUtf8Text
    assert $ txt == decodeUtf8 @_ @ByteString  (encodeUtf8 txt)
          && txt == decodeUtf8 @_ @LByteString (encodeUtf8 txt)

-- "\239\191\190" fails, but this is the same as "\65534" :: String
prop_BytesTo :: Property
prop_BytesTo = property $ do
    utf <- forAll genUtf8ByteString
    assert $ utf == encodeUtf8 @String (decodeUtf8 utf)
          && utf == encodeUtf8 @Text   (decodeUtf8 utf)
          && utf == encodeUtf8 @LText  (decodeUtf8 utf)

----------------------------------------------------------------------------
-- ordNub
----------------------------------------------------------------------------

listProps :: Group
listProps = Group "list function property tests"
    [ ("ordNub xs == nub xs:", prop_ordNubCorrect)
    , ("hashNub xs == nub xs:", prop_hashNubCorrect)
    , ("sortNub xs == sort (nub xs):" , prop_sortNubCorrect)
    , ("sort (unstableNub xs) == sort (nub xs):" , prop_unstableNubCorrect)
    ]

prop_ordNubCorrect :: Property
prop_ordNubCorrect = property $ do
    xs <- forAll genIntList
    ordNub xs === nub xs

prop_hashNubCorrect :: Property
prop_hashNubCorrect = property $ do
    xs <- forAll genIntList
    hashNub xs === ordNub xs

prop_sortNubCorrect :: Property
prop_sortNubCorrect = property $ do
    xs <- forAll genIntList
    sortNub xs === sort (ordNub xs)

prop_unstableNubCorrect :: Property
prop_unstableNubCorrect = property $ do
    xs <- forAll genIntList
    sort (unstableNub xs) === sortNub xs

----------------------------------------------------------------------------
-- logicM
----------------------------------------------------------------------------

logicProps :: Group
logicProps = Group "lifted logic function property tests"
    [ ("andM:", prop_andM)
    , ("orM:", prop_orM)
    ]

prop_andM :: Property
prop_andM = property $ do
    bs <- forAll genBoolList
    andM (pure <$> bs) === pure @Maybe (and bs)

prop_orM :: Property
prop_orM = property $ do
    bs <- forAll genBoolList
    orM (pure <$> bs) === pure @Maybe (or bs)
