import Test.Hspec

import qualified KeySpec

main :: IO ()
main = hspec $
  describe "Key" KeySpec.spec
