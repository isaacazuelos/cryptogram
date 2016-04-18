-- |
-- Module      : Toy.Cryptogram.Dictionary
-- Description : Cryptogram Dictionary
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Toy.Cryptogram.Dictionary
    ( Dictionary (Dictionary)
    , empty
    , fromWords
    , toWords
    , lookup
    , fingerprint
    )
  where

import Prelude hiding (lookup)

import qualified Data.Text as Text

-- | The default dictionary location.
defaultPath :: FilePath
defaultPath = "/usr/share/dict/words"

-- | A dictionary is a data structure lets us look up all words which could
-- concieveably map to eachother by some substitution cypher.
data Dictionary = Dictionary () deriving (Eq)

instance Show Dictionary where
  show = (++) "toWords " . show . toWords

empty :: Dictionary
empty = undefined

-- | Look up a word in the dictionary, to get all other words which it could be
-- under application of some key.
lookup :: Dictionary -> Text.Text -> [Text.Text]
lookup d t = undefined

-- | Build a dictionary from a list of words.
fromWords :: [Text.Text] -> Dictionary
fromWords ws = undefined

-- | Pull out all the words in a @Dictionary@.
toWords :: Dictionary -> [Text.Text]
toWords d = undefined

newtype Fingerprint = FP () deriving (Show, Eq)

fingerprint :: Text.Text -> Maybe Fingerprint
fingerprint t = undefined
