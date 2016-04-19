{-# LANGUAGE OverloadedStrings #-}

module CryptogramSpec where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Text          as Text

import qualified Toy.Cryptogram     as Crypto
import qualified Toy.Cryptogram.Key as Key

-- I hope it doesn't generate infinite strings...
instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

-- Test data generated with the help of <http://www.quipqiup.com>
rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

spec =
  describe "encrypt and decrypt" $ do
    it "should handle uppercase ascii strings properly." $ do
      let Just key   = Key.parse "DBNHRZOYMLKSQAFCWXVEGTUIJP"
      let plaintext  = "SUPERCALIFRAGILISTICEXPIALIDOCIOUS"
      let cyphertext = "VGCRXNDSMZXDOMSMVEMNRICMDSMHFNMFGV"
      Crypto.encrypt key plaintext `shouldBe` cyphertext
      Crypto.decrypt key cyphertext `shouldBe` plaintext
    it "shouldn't affect non-ASCII-uppercase characters" $ do
      let Just key   = Key.parse "WCLTJNXYPSEFQIHMDUKAOBVZRG"
      let plaintext  = "TEST MESSAGE, PLEASE IGNORE."
      let cyphertext = "AJKA QJKKWXJ, MFJWKJ PXIHUJ."
      Crypto.encrypt key plaintext  `shouldBe` cyphertext
      Crypto.decrypt key cyphertext `shouldBe` plaintext
    it "should handle partial keys" $ do
      let Just key = Key.parse "***DEFGHIJKLMNOPQRSTUVWXYZ"
      Crypto.encrypt key alphabet `shouldBe` "***DEFGHIJKLMNOPQRSTUVWXYZ"
      Crypto.decrypt key alphabet `shouldBe` "***DEFGHIJKLMNOPQRSTUVWXYZ"
    it "should undo each other" $ do
      -- I'd rather use Key.generateRandom, but I can't get IO to work
      -- nicely with quickcheck
      let Just key = Key.parse "WCLTJNXYPSEFQIHMDUKAOBVZRG"
      property $ \t -> Crypto.decrypt key (Crypto.encrypt key t) `shouldBe` t
