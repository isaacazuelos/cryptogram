import Test.Hspec

import qualified KeySpec
import qualified DictionarySpec

main :: IO ()
main = hspec $
  describe "Toy.Cryptogram" $ do
   KeySpec.spec
   DictionarySpec.spec
