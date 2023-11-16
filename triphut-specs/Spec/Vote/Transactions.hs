module Spec.Vote.Transactions (runInitVoteConfig) where

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
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1)
import Spec.SampleData (sampleVoteDynamicConfig)
import Spec.SpecUtils (initScriptRef, minAda)
import Spec.Values (dummyVoteConfigNftValue)
import Prelude ((<>))

runInitVoteConfig :: Run ()
runInitVoteConfig = do
  initAlwaysSucceedScriptRef
  admin <- getMainUser
  let configVal = dummyVoteConfigNftValue <> minAda
  spend' <- spend admin configVal
  let payTx = payToRef alwaysSucceedTypedValidator1 (InlineDatum sampleVoteDynamicConfig) configVal
  submitTx admin $ userSpend spend' <> payTx

initAlwaysSucceedScriptRef :: Run ()
initAlwaysSucceedScriptRef = initScriptRef alwaysSucceedTypedValidator1
