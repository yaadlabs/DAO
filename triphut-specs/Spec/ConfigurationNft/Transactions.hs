module Spec.ConfigurationNft.Transactions (runInitConfig) where

import Plutus.Model (
  Run,
  getMainUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToRef,
 )
import PlutusTx.Prelude (($))
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator)
import Spec.SampleData (sampleDynamicConfig)
import Spec.SpecUtils (initScriptRef, minAda)
import Spec.Values (dummyConfigNftValue)
import Prelude ((<>))

runInitConfig :: Run ()
runInitConfig = do
  initAlwaysSucceedScriptRef
  admin <- getMainUser
  let configVal = dummyConfigNftValue <> minAda
  spend' <- spend admin configVal
  let payTx = payToRef alwaysSucceedTypedValidator (InlineDatum sampleDynamicConfig) configVal
  submitTx admin $ userSpend spend' <> payTx

initAlwaysSucceedScriptRef :: Run ()
initAlwaysSucceedScriptRef = initScriptRef alwaysSucceedTypedValidator
