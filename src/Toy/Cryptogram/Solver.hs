-- |
-- Module      : Toy.Cryptogram.Solver
-- Description : Cryptogram solver
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A depth-first search solver, using dictionaries to restrict possible words.

module Toy.Cryptogram.Solver
    ( solutions
    , expandKey
    , usable
    )
  where

import           Data.Maybe                (mapMaybe)

import qualified Data.Char                 as Char
import qualified Data.Text                 as Text
import qualified Toy.Cryptogram.Dictionary as Dict
import qualified Toy.Cryptogram.Key        as Key

-- | Returns the possible keys used to encrypt some cyphertext.
solutions :: Dict.Dictionary -> Text.Text -> [Key.Key]
solutions dict ct = go (filter usable (Text.words ct)) Key.empty
  where
    go [] k = [k]
    go (w:ws) key = expandKey dict key w >>= go ws

-- | Can some word be used by the solver?
usable :: Text.Text -> Bool
usable = Text.all Char.isAsciiUpper

-- | Exapdn a key using the candidates for a cypher-word in a dictionary
expandKey :: Dict.Dictionary -> Key.Key -> Text.Text -> [Key.Key]
expandKey d k w =
  if usable w
    then mapMaybe (flip (Key.insert k) w) (Dict.lookup d w)
    else mempty
