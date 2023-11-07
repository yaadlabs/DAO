module Spec.SpecUtils (checkFails) where

import Plutus.Model (
  MockConfig,
  Run,
  mustFail,
  skipLimits,
  testNoErrors,
 )
import Plutus.V1.Ledger.Value (Value)
import Test.Tasty (TestTree)
import Prelude (String)

checkFails :: MockConfig -> Value -> String -> Run () -> TestTree
checkFails cfg funds msg act =
  testNoErrors funds (skipLimits cfg) msg (mustFail act)
