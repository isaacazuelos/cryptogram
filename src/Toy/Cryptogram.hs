-- |
-- Module      : Toy.Cryptogram
-- Description : Cryptograms
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A library for working with cryptograms, specifically the ones in
-- Clever Cryptograms by Louise B. Moll.
--
-- A cryptogram is some text encoded by a substitution cypher. Like Moll, we're
-- encoding only uppercase English letter --- any other characters are left
-- unchanged.

module Toy.Cryptogram
    ( encrypt
    , decrypt
    )
  where

import qualified Data.Text as Text
import qualified Toy.Cryptogram.Key as Key

-- | Encrypt some plaintext using a known key.
encrypt :: Key.Key -> Text.Text -> Text.Text
encrypt = Key.apply

-- | Decrypt some cyphertext uisng a known key.
decrypt :: Key.Key -> Text.Text -> Text.Text
decrypt = Key.apply . Key.inverse
