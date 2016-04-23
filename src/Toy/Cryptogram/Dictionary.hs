-- |
-- Module      : Toy.Cryptogram.Dictionary
-- Description : Cryptogram Dictionary
-- License     : MIT License
-- Maintainer  : Isaac Azuelos

{-# LANGUAGE OverloadedStrings #-}

module Toy.Cryptogram.Dictionary
    ( Dictionary (Dictionary)
    , defaultPath
    , empty
    , fromWords
    , toWords
    , lookup
    , Fingerprint
    , fingerprint
    )
  where

import           Prelude     hiding (lookup)

import           Data.Maybe  (fromMaybe)

import qualified Data.Char   as Char
import qualified Data.Map    as Map
import qualified Data.Text   as Text
import qualified Data.Vector as Vector

-- | The default dictionary location.
defaultPath :: FilePath
defaultPath = "/usr/share/dict/words"

-- | A dictionary is a data structure lets us look up all words which could
-- concieveably map to eachother by some substitution cypher.
data Dictionary = Dictionary (Map.Map Fingerprint [Text.Text]) deriving (Eq)

instance Show Dictionary where
  show = (++) "toWords " . show . toWords

-- | An empty dictionary containing no words.
empty :: Dictionary
empty = Dictionary mempty

-- | Look up a word in the dictionary, to get all other words which it could be
-- under application of some key.
lookup :: Dictionary -> Text.Text -> [Text.Text]
lookup (Dictionary m) t = filter (/= t)
                        $ fromMaybe [] (fingerprint t >>= flip Map.lookup m)

-- | Build a dictionary from a list of words.
fromWords :: [Text.Text] -> Dictionary
fromWords ws = Dictionary $ foldr insertWord mempty ws
  where insertWord w m = case fingerprint w of
                          Nothing -> m
                          Just f  -> Map.insertWith mappend f (return w) m

-- | Pull out all the words in a @Dictionary@.
toWords :: Dictionary -> [Text.Text]
toWords (Dictionary d) = concat $ Map.elems d

-- | Fingerprints are just a vector where each character is mapped to the index
-- of it's first appearence.
newtype Fingerprint = FP (Vector.Vector Int) deriving (Show, Eq, Ord)

-- | Each word has a fingerprint, but they're not all unique. Two words which
-- can be made the same by the applicaiton of some key have the same
-- fingerprint.
fingerprint :: Text.Text -> Maybe Fingerprint
fingerprint t = FP <$> Vector.foldr ((=<<) . append) (Just mempty) chars
  where
    chars = Vector.fromList (Text.unpack t)
    append c v
      | Char.isAsciiUpper c = flip Vector.cons v <$> Vector.elemIndex c chars
      | otherwise = Nothing
