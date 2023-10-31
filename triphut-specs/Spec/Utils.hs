module Spec.Utils (spec) where

import Prelude (($))
import Spec.Utils.Unit (unitSpec)
import Test.Tasty (TestTree, testGroup, adjustOption)
import Test.Tasty.HUnit (assertBool, testCase)

-- | Utility functions tests
spec :: TestTree
spec =
    testGroup
      "Utils"
      unitSpec
