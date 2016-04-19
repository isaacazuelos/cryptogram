import Test.Hspec

import qualified KeySpec
import qualified DictionarySpec

main :: IO ()
main = hspec $
  describe "Toy.Cryptogram" $ do
   describe "Key" KeySpec.spec
   describe "Dictionary" DictionarySpec.spec
