{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}

module DictionarySpec where

import Test.Hspec
import Test.QuickCheck as QC

import Prelude hiding (words)

import Data.Monoid ((<>))
import Data.Maybe (isNothing)

import qualified Toy.Cryptogram.Dictionary as Dict

import qualified Data.Text as T

-- I hope it doesn't generate infinite strings...
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

alphabet :: T.Text
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

words :: [T.Text]
words = [ "CONCUR",       "CONCURRED",     "CONCURRENCE"
        , "CONCURRENCES", "CONCURRENCIES", "CONCURRENCY"
        , "CONCURRENT",   "CONCURRENTLY",  "CONCURRENTS"
        , "CONCURRING",   "CONCURRINGLY",  "CONCURS"
        ]

invalids :: [T.Text]
invalids = ["HE'LL", "*#^&%*^%&(^%)", "mostly-valid"]

spec = do
  describe "withWords and toWords" $ do
    it "build dictionaries with words" $ do
      Dict.toWords (Dict.fromWords []) `shouldBe` []
      let valid = ["HELLO", "BYE"]
      Dict.toWords (Dict.fromWords valid) `shouldMatchList` valid
    it "should remove non-ASCII-uppercase-containing words" $ do
      Dict.toWords (Dict.fromWords ["hello"]) `shouldBe` []
      Dict.toWords (Dict.fromWords ["HElLO"]) `shouldBe` []
      Dict.toWords (Dict.fromWords ["HE'LL"]) `shouldBe` []
      Dict.toWords (Dict.fromWords ["*", "A"]) `shouldBe` ["A"]

  describe "empty" $ do
    it "nothing should be be returned from any lookup" $
      property $ null . Dict.lookup Dict.empty
    it "should contain no words" $ do
      Dict.toWords Dict.empty `shouldBe` []
      Dict.fromWords [] `shouldBe` Dict.empty

  describe "fingerprint" $ do
    it "should properly identify like fingerprints" $ do
      Dict.fingerprint "AAA" `shouldBe` Dict.fingerprint "AAA"
      Dict.fingerprint "AAA" `shouldBe` Dict.fingerprint "BBB"
      Dict.fingerprint "A-A" `shouldBe` Nothing
      Dict.fingerprint "A-A" `shouldBe` Dict.fingerprint "B-B"
    it "should properly identify unalike fingerprints" $ do
      Dict.fingerprint "AZA" `shouldBe` Dict.fingerprint "AAA"
      Dict.fingerprint "AAA" `shouldNotBe` Dict.fingerprint "ABA"
      Dict.fingerprint "A-A" `shouldBe` Nothing
    it "should be case sensitive" $ do
      Dict.fingerprint "aaa" `shouldNotBe` Dict.fingerprint "AAA"
      Dict.fingerprint "AAA" `shouldNotBe` Dict.fingerprint "aaa"

  describe "lookup" $ do
    let dict = Dict.fromWords words
    it "should return nothing for invalid words" $
      mapM_ (\ i -> Dict.lookup dict i `shouldBe` []) invalids
    it "should not return the word looked for" $
      mapM_ (\ w -> Dict.lookup dict w `shouldNotContain` [w]) words
    it "should only return words with the same fingerprint" $ do
      let dict = Dict.fromWords ["BBB", "AAA", "CCC", "ABA"]
      Dict.lookup dict "AAA" `shouldNotContain` ["ABA"]
      Dict.lookup dict "ABA" `shouldBe` []
