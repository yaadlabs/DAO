module Spec.SpecUtils (checkFails, mkTypedValidator') where

import Plutus.Model (
  MockConfig,
  Run,
  mustFail,
  skipLimits,
  testNoErrors,
 )
import Plutus.Model.V2 (
  TypedValidator (TypedValidator),
  toV2,
 )
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Value (Value)
import PlutusTx.Prelude (($), (.))
import Test.Tasty (TestTree)
import Prelude (String)

checkFails :: MockConfig -> Value -> String -> Run () -> TestTree
checkFails cfg funds msg act =
  testNoErrors funds (skipLimits cfg) msg (mustFail act)

mkTypedValidator' :: config -> (config -> Validator) -> TypedValidator datum redeemer
mkTypedValidator' config mkValidator = TypedValidator . toV2 $ mkValidator config
