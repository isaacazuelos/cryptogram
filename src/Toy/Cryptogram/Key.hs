-- |
-- Module      : Toy.Cryptogram.Key
-- Description : Cryptogram keys
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A key is a mapping from letters of the alphabet to others. Keys can be
-- partial, since we won't always know how certain letters are mapped if they're
-- not used in a puzzle.
--
-- The mapping is in the direction used to encode the text, i.e. encoding of
-- "abc" with the key starting with "ysa" is "ysa".

{-# LANGUAGE OverloadedStrings #-}

module Toy.Cryptogram.Key
    ( Key
    , empty
    , identity
    , inverse
    , apply
    , parse
    , solves
    , insert
    , humanReadable
    , generateRandom
    )
  where

import           Prelude       hiding (lookup)

import           Data.Char     (chr, isAsciiUpper, ord)
import           System.Random (newStdGen, randoms)

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Vector   as Vector

-- | A key is a mapping of the letters in the plaintext to the letters in the
-- cyphertext.
--
-- Implementation-wise, we're using a letter's place in the alphabet as an index
-- into a vector to access the character mapped to.
--
-- So mapping @'a'@ to @'c'@ would set the first character (the character
-- @indexBy@ed  @'a'@) to @'c'@.
newtype Key = Key (Vector.Vector Char) deriving (Show, Eq)

-- |  Turn a key into human-readable text.
humanReadable :: Key -> Text.Text
humanReadable = toText

-- | Convert a key into a @Text.Text@ representation.
toText :: Key -> Text.Text
toText (Key k) = Text.pack (Vector.toList k)

-- | An empty key, in which no mappings are know. Applying this key to any
-- message will leave it unchanged.
empty :: Key
empty = Key (Vector.replicate 26 '*')

-- | The key that does nothing when applied.
identity :: Key
identity = Key (Vector.fromList ['A'..'Z'])

-- | Text is solved by a key when there are no letters in the text which
-- unknown in the key.
solves :: Key -> Text.Text -> Bool
solves k t = Text.all ((/=) '*' . lookup k) (Text.filter isAsciiUpper t)

-- | The inverse of a key is an inverse in the usual mathematical sense.
inverse :: Key -> Key
inverse (Key v) = foldr (uncurry (flip insertSwap')) empty (action v)
  where
    action = filter ((/=) '*' . snd)
           . zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           . Vector.toList

-- | Generates a random key. There are never any unknown letters in the
-- generated key, i.e. the key is @complete@
generateRandom :: IO Key
generateRandom = ( Key
                 . Vector.fromList
                 . map (chr . (ord 'A' +))
                 . take 26
                 . List.nub
                 . map (`mod` 26)
                 . randoms
                 ) <$> newStdGen

-- | Parse some text into a key --- `Nothing` means no key could be parsed.
--
-- @
--   prettify <$> parse == return
-- @
parse :: Text.Text -> Maybe Key
parse t
    | Text.length t   /= 26     = Nothing
    | List.nub knowns /= knowns = Nothing
    | Text.any (not . isValidChar) t = Nothing
    | otherwise = Just . Key . Vector.fromList $ str
  where str = Text.unpack t
        knowns = filter (/= '*') str
        isValidChar c = isAsciiUpper c || c == '*'

-- | Look up a plaintext character to know what it maps to.
lookup :: Key -> Char -> Char
lookup (Key v) c
  | isAsciiUpper c = v Vector.! indexBy c
  | c == '*' = '*'
  | otherwise = c

-- | The zero-based index of the character in the alphabet.
indexBy :: Char -> Int
indexBy c = ord c - ord 'A'

-- | Insert the swaps implied by a word-to-word mapping into a key, returning
-- Nothing if a conflict arises.
insert :: Key -> Text.Text -> Text.Text -> Maybe Key
insert k pt ct
  | Text.length ct /= Text.length pt = Nothing
  | otherwise = foldr ((=<<) . insertSwap) (Just k) (swaps pt ct)

-- | A swap is the mapping of one character to another.
type Swap = (Char, Char) -- (PT, CT)

-- | Returns a list of the swaps implied by mapping some cyphertext into some
-- plaintext. Swaps that are mutually exclusive can appear, but invalid
-- characters won't appear.
swaps :: Text.Text -> Text.Text -> [Swap]
swaps ct pt = zip (Text.unpack ct)
                  (Text.unpack pt)

-- | Inserts a single swap into a key, if it wouldn't cause a problem.
--
-- We can only insert a swap into the key if:
--
-- 0. Both letters from and to are valid mappings.
-- 1. The letter being mapped from isn't alrady set **to something else**.
-- 2. The letter being mapped to isn't alreay mapped to.
insertSwap :: Swap -> Key -> Maybe Key
insertSwap (f, t) k
    -- neither letter is valid, so don't change key
    | not (isAsciiUpper f) && not (isAsciiUpper t) = return k
    -- only one letter is valid, so key is invalid.
    | not (isAsciiUpper f) &&      isAsciiUpper t  = Nothing
    |      isAsciiUpper f  && not (isAsciiUpper f) = Nothing
    -- That letter is alreay mapped to that target
    | lookup k f == t                              = return k
    -- That letter is alreay mapped to something else
    | isAsciiUpper (lookup k f)                    = Nothing
    -- The letter is mapped to by something else
    | let (Key v) = k in t `Vector.elem` v         = Nothing
    -- insert the swap
    | lookup k f == '*'                            = return $ insertSwap' f t k
    -- Anything else is a mistake.
    | otherwise                                    = Nothing

-- | Forces the insertion of a swap into a key, explodes on failure.
insertSwap' :: Char -> Char -> Key -> Key
insertSwap' f t (Key v) = Key (v Vector.// [(indexBy f, t)])

-- | Apply a key to some text
apply :: Key -> Text.Text -> Text.Text
apply = Text.map . lookup
