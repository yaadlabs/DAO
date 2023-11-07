module Spec.SpecUtils (checkFails) where

import Prelude (String)
import Test.Tasty (TestTree)
import Plutus.V1.Ledger.Value (Value)
import Plutus.Model 
  ( MockConfig
  , Run
  , testNoErrors
  , mustFail
  , skipLimits
  )

checkFails :: MockConfig -> Value -> String -> Run () -> TestTree
checkFails cfg funds msg act =
  testNoErrors funds (skipLimits cfg) msg (mustFail act)
