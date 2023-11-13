module Spec.Tally.Transactions (runInitTally) where

import Plutus.Model (
  Run,
  getMainUser,
  payToScript,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
 )
import PlutusTx.Prelude (($))
import Spec.SpecUtils (minAda)
import Spec.Tally.SampleData (sampleTallyStateDatum)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyTallyValue)
import Prelude ((<>))

runInitTally :: Run ()
runInitTally = do
  admin <- getMainUser
  let tallyValue = dummyTallyValue <> minAda
  spend' <- spend admin tallyValue
  let payTx = payToScript tallyNftTypedValidator (InlineDatum sampleTallyStateDatum) tallyValue
  submitTx admin $ userSpend spend' <> payTx
