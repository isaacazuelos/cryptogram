{-# LANGUAGE OverloadedStrings #-}

module KeySpec where

import Test.Hspec
import Test.QuickCheck as QC

import Data.Monoid ((<>))
import Data.Maybe (isNothing)

import qualified Toy.Cryptogram.Key as Key

import qualified Data.Text as T

-- I hope it doesn't generate infinite strings...
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

alphabet :: T.Text
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

spec = do
  describe "empty" $ do
    it "should be all stars" $
      Key.humanReadable Key.empty  `shouldBe` "**************************"
    it "should turn text applied to into stars" $
      Key.apply Key.empty alphabet `shouldBe` "**************************"

  describe "identity" $ do
    it "should be the alphabet" $
      Key.humanReadable Key.identity `shouldBe` alphabet
    it "should be the identity for `apply`" $
      property $ \t -> Key.apply Key.identity t == t

  describe "generateRandom" $ do
    it "should generate a random total key" $ do
      key <- Key.generateRandom
      T.unpack (Key.humanReadable key) `shouldNotContain` ['*']
    it "should not create the same key every time" $ do
      key1 <- Key.generateRandom
      key2 <- Key.generateRandom
      -- while technically possible, the odds are not it its favour
      key1 `shouldNotBe` key2

  describe "parse" $ do
    it "should parse the alphabet correctly" $
      Key.humanReadable <$> Key.parse alphabet `shouldBe` Just alphabet
    it "should require the correct lenght" $ do
      Key.parse "" `shouldBe` Nothing
      Key.parse "****************************************" `shouldBe` Nothing
    it "should parse '*' properly as any character" $
      Key.parse "**************************" `shouldBe` Just Key.empty
    it "should detect repeated characters in keys" $
      Key.parse "***A***************A******" `shouldBe` Nothing
    it "should detect invalid characters" $ do
      Key.parse "***********a**************" `shouldBe` Nothing
      Key.parse "*******#***********A******" `shouldBe` Nothing

  describe "apply" $ do
    let pt = "TEST MESSAGE"
    let ct = "JNZJ PNZZHMN"
    let (Just k) = Key.parse "HGDCNXMULWETPYRAQVZJSKIBFO"
    it "should properly apply total keys" $
      Key.apply k pt `shouldBe` ct
    it "should properly apply keys with some '*'s." $ do
      let (Just k') = Key.parse "HGDCN*MULWETPYRAQVZJSKIBFO"
      Key.apply k' pt `shouldBe` ct
    it "should properly apply keys over text with invalid characters" $ do
      let pt' = "tEST-MESSAGE!"
      let ct' = "tNZJ-PNZZHMN!"
      Key.apply k pt' `shouldBe` ct'

  describe "insert" $ do
    it "should do nothing when inserting empty strings" $ do
      key <- Key.generateRandom
      Key.insert key "" "" `shouldBe` Just key
    it "should not allow insertion of strings of different lengths" $ do
      Key.insert Key.empty "" "asdf" `shouldBe` Nothing
      Key.insert Key.empty "jkl;" "" `shouldBe` Nothing
    let intoEmpty = Key.insert Key.empty
    it "should handle invalid characters" $ do
      intoEmpty "A_A" "BBB" `shouldBe` Nothing
      intoEmpty "A_A" "B_B" `shouldBe` Key.parse "B*************************"
    it "should build keys out properly" $ do
      intoEmpty "AB"  "BC"  `shouldBe` Key.parse "BC************************"
      intoEmpty "ABA" "BCB" `shouldBe` Key.parse "BC************************"
    it "should detect conflicts in the messages" $ do
      intoEmpty "ABA" "BCD" `shouldBe` Nothing
      intoEmpty "AAA" "BCB" `shouldBe` Nothing
    it "should detect conflicts with the key" $ do
      let (Just key) = Key.parse "B*************************"
      Key.insert key "A" "C" `shouldBe` Nothing
      Key.insert key "Z" "B" `shouldBe` Nothing

  describe "solves" $ do
    it "should know when a total key solves any text" $
      property $ Key.solves Key.identity
    let (Just key) = Key.parse "***DEFGHIJKLMNOPQRSTUVWXYZ"
    it "should know when a partial key doesn't solve some text" $
      "ABC" `shouldNotSatisfy` Key.solves key
    it "should know when a partial key does solve some text" $
      "XYZ" `shouldSatisfy` Key.solves key
    it "should handle invalid characters" $
      "W*X-Y'Z" `shouldSatisfy` Key.solves key

  describe "inverse" $ do
    it "should do nothing to the identity key" $
      Key.inverse Key.identity `shouldBe` Key.identity
    it "should do nothing to the empty key" $
      Key.inverse Key.empty `shouldBe` Key.empty
    it "should produce an inverse of total keys" $ do
      let Just shiftRight3 = Key.parse "DEFGHIJKLMNOPQRSTUVWXYZABC"
      let Just shiftLeft3  = Key.parse "XYZABCDEFGHIJKLMNOPQRSTUVW"
      Key.inverse shiftLeft3 `shouldBe` shiftRight3
      Key.inverse shiftRight3 `shouldBe` shiftLeft3
      True `shouldBe` True
    it "should produce an inverse of partial keys" $ do
      let Just key1 = Key.parse "BCA***********************"
      let Just key2 = Key.parse "CAB***********************"
      Key.inverse key1 `shouldBe` key2
      Key.inverse key2 `shouldBe` key1
    it "applied twice should do nothing to total keys" $ do
      key <- Key.generateRandom
      Key.inverse (Key.inverse key) `shouldBe` key
