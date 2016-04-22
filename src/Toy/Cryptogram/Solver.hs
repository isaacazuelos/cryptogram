-- |
-- Module      : Toy.Cryptogram.Solver
-- Description : Cryptogram solver
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Toy.Cryptogram.Solver
    ( solutions
    , expandKey
    , usable
    )
  where

import           Data.Ord                  (comparing)

import qualified Data.Char                 as Char
import qualified Data.List                 as List
import qualified Data.Text                 as Text
import qualified Toy.Cryptogram.Dictionary as Dict
import qualified Toy.Cryptogram.Key        as Key


solutions :: Dict.Dictionary -> Text.Text -> [Key.Key]
solutions dict ct = undefined

usable :: Text.Text -> Bool
usable = undefined

expandKey :: Dict.Dictionary -> Key.Key -> Text.Text -> [Key.Key]
expandKey d k w = undefined
