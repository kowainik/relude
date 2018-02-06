
module Test.Universum.Property
        ( hedgehogTestTree
        ) where

import Universum
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
-- import qualified Data.ByteString.UTF8      as BU
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as LT

import Data.List (nub)


hedgehogTestTree :: TestTree
hedgehogTestTree = testGroup "Tests" [utfProps, listProps, boolMProps]

utfProps :: TestTree
utfProps = testGroup "utf8 conversion property tests" 
    [ testProperty "String to ByteString invertible" prop_StringToBytes
    , testProperty "Text to ByteString invertible" prop_TextToBytes
    , testProperty "ByteString to Text or String invertible" prop_BytesTo
    ]

unicode' :: MonadGen m => m Char
unicode' = do
    a <- Gen.unicode
    if Universum.elem a ['\65534', '\65535']
    then unicode'
    else return a

utf8String :: Gen String
utf8String = Gen.string (Range.linear 0 10000) unicode'

utf8Text :: Gen T.Text
utf8Text = Gen.text (Range.linear 0 10000) unicode'

utf8Bytes :: Gen B.ByteString
utf8Bytes = Gen.utf8 (Range.linear 0 10000) unicode'

-- "\65534" fails, but this is from BU.toString
-- > BU.toString (BU.fromString "\65534") == "\65533"

prop_StringToBytes :: Property
prop_StringToBytes = property $ do
    str <- forAll utf8String
    assert $ str == (decodeUtf8 (encodeUtf8 str :: B.ByteString))
          && str == (decodeUtf8 (encodeUtf8 str :: LB.ByteString))


prop_TextToBytes :: Property
prop_TextToBytes = property $ do
    txt <- forAll utf8Text
    assert $ txt == (decodeUtf8 (encodeUtf8 txt :: B.ByteString))
          && txt == (decodeUtf8 (encodeUtf8 txt :: LB.ByteString))

-- "\239\191\190" fails, but this is the same as "\65534" :: String
prop_BytesTo :: Property
prop_BytesTo = property $ do
    utf <- forAll utf8Bytes
    assert $ utf == (encodeUtf8 (decodeUtf8 utf :: String))
          && utf == (encodeUtf8 (decodeUtf8 utf :: T.Text))
          && utf == (encodeUtf8 (decodeUtf8 utf :: LT.Text))

-- ordNub

listProps :: TestTree
listProps = testGroup "list function property tests" 
    [ testProperty "Hedgehog ordNub xs == nub xs" prop_ordNubCorrect
    ]

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 10000) Gen.enumBounded

prop_ordNubCorrect :: Property
prop_ordNubCorrect = property $ do
    xs <- forAll genIntList
    ordNub xs === Data.List.nub xs

-- logicM

genBoolList :: Gen [Bool]
genBoolList = Gen.list (Range.linear 0 1000) Gen.bool

boolMProps :: TestTree
boolMProps = testGroup "lifted logic function property tests"
    [ testProperty "Hedgehog andM" prop_andM
    , testProperty "Hedgehog orM" prop_orM
    ]

prop_andM :: Property
prop_andM = property $ do
    bs <- forAll genBoolList
    andM (return <$> bs) === ((return $ Universum.and bs) :: Maybe Bool)

prop_orM :: Property
prop_orM = property $ do
    bs <- forAll genBoolList
    orM (return <$> bs) === ((return $ Universum.or bs) :: Maybe Bool)
