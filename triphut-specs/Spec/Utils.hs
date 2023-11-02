module Spec.Utils (spec) where

import Spec.Utils.Unit (unitSpec)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Prelude (($))

-- | Utility functions tests
spec :: TestTree
spec =
  testGroup
    "Utils"
    unitSpec
