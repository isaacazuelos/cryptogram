module Toy.Cryptogram
    ( encrypt
    , decrypt
    )
  where

import qualified Data.Text as Text
import qualified Toy.Cryptogram.Key as Key

encrypt :: Key.Key -> Text.Text -> Text.Text
encrypt = Key.apply

decrypt :: Key.Key -> Text.Text -> Text.Text
decrypt = Key.apply . Key.inverse
