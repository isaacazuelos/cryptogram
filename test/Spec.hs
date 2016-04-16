import Test.Hspec

import qualified KeySpec

main :: IO ()
main = hspec $ describe "Toy.Cryptogram" KeySpec.spec
