import           Test.Hspec

import qualified CryptogramSpec
import qualified DictionarySpec
import qualified KeySpec
import qualified SolverSpec

main :: IO ()
main = hspec $
  describe "Toy.Cryptogram" $ do
    describe "Key" KeySpec.spec
    describe "Dictionary" DictionarySpec.spec
    describe "Cryptogram" CryptogramSpec.spec
    describe "Solver" SolverSpec.spec
