module Spec.Utils (spec) where

import Spec.Utils.Unit (unitSpec)
import Test.Tasty (TestTree, testGroup)

-- | Utility functions tests
spec :: TestTree
spec =
  testGroup
    "Utils"
    unitSpec
