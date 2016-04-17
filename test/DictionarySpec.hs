{-# LANGUAGE OverloadedStrings #-}

module DictionarySpec where

import Test.Hspec
import Test.QuickCheck as QC

import Data.Monoid ((<>))
import Data.Maybe (isNothing)

import qualified Toy.Cryptogram.Dictionary as Dictionary

import qualified Data.Text as T

-- I hope it doesn't generate infinite strings...
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

alphabet :: T.Text
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

spec = describe "Dictinary" $
  describe "There are no tests implemented yet." $
    return ()
