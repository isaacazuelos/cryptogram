{-# LANGUAGE OverloadedStrings #-}

module SolverSpec where

import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Text                 as Text

import qualified Toy.Cryptogram.Dictionary as Dict
import qualified Toy.Cryptogram.Key        as Key
import qualified Toy.Cryptogram.Solver     as Solver

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

spec = do
  describe "usable" $ do
    it "should allow words usable by the solver" $ do
      "" `shouldSatisfy` Solver.usable
      "TEST" `shouldSatisfy` Solver.usable
    it "should reject other characters" $
      mapM_ (`shouldNotSatisfy` Solver.usable)
        ["-", "test", "HI THERE", "*WORD"]

  describe "expandKey" $ do
    it "should only allow usable words to expand a key" $ do
      let dict = Dict.fromWords ["A", "B", "HI", "MY"]
      Solver.expandKey dict Key.empty "A"  `shouldNotBe` []
      Solver.expandKey dict Key.empty "AB" `shouldNotBe` []
      Solver.expandKey dict Key.empty "a"  `shouldBe` []
      Solver.expandKey dict Key.empty "_+" `shouldBe` []
    it "should expand a key based on what it could map to" $ do
      let dict = Dict.fromWords ["MY", "PI"]
      Solver.expandKey dict Key.empty "AB" `shouldMatchList`
        mapMaybe Key.parse
          [ "************A***********B*" -- MY
          , "********B******A**********" -- PI
          ]
    it "should recognize when a word produces no candidates" $
      property $ \t -> Solver.expandKey Dict.empty Key.empty t `shouldBe` []
    it "should remove non-viable candidates" $ do
      let dict = Dict.fromWords ["HI"]
      -- The key here encodes "PIE" as "ABC"
      let Just keyBefore = Key.parse "****C***B******A**********"
      -- So "AB" must be "PI" and can't be "HI".
      Solver.expandKey dict keyBefore "AB" `shouldBe` []

  describe "solutions" $ do
    it "should return the key as needed to encrypt, not decrypt" $ do
      -- If the only one letter word is "A", then the cyphertext word "Z"
      -- must map to "A". That means the key would map "A" to "Z".
      let word = "Z"
      let dict = Dict.fromWords ["A"]
      let Just key = Key.parse "Z*************************"
      Solver.solutions dict word `shouldContain` [key]
    it "should infer the keys induced by a single word" $ do
      let word = "ISAAC"
      let dict = Dict.fromWords ["HELLO", "SPEED", "NOPE", "NOTTHISONE"]
      let Just key = Key.parse "****S**I***A**C***********"
      Solver.solutions dict word `shouldContain` [key]
    it "should detect when there aren't any similar words" $
      Solver.solutions Dict.empty "UNIQUE" `shouldBe` []
    it "should chain together multiple words correctly" $ do
      let text = "ISAAC BCDS"
      let dict = Dict.fromWords ["HELLO", "SPEED", "NOPE", "NOTTHISONE"]
      let Just key = Key.parse "****S**I***A*BCD**********"
      Solver.solutions dict text `shouldContain` [key]
    it "should detect when a word rules out a key" $ do
      let text = "ISAAC FAIL"
      let dict = Dict.fromWords ["HELLO", "SPEED", "NOPE", "NOTTHISONE"]
      Solver.solutions dict text `shouldBe` []  
