module Spec.Index.Transactions (runInitIndex) where

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
import Spec.Index.SampleData (validSampleIndexNftDatum)
import Spec.Index.Script (indexNftTypedValidator)
import Spec.SpecUtils (minAda)
import Spec.Values (dummyIndexConfigNftValue)
import Prelude ((<>))

runInitIndex :: Run ()
runInitIndex = do
  admin <- getMainUser
  let indexConfigValue = dummyIndexConfigNftValue <> minAda
  spend' <- spend admin indexConfigValue
  let payTx = payToScript indexNftTypedValidator (InlineDatum validSampleIndexNftDatum) indexConfigValue
  submitTx admin $ userSpend spend' <> payTx
