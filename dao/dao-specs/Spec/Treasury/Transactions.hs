module Spec.Treasury.Transactions (
  runInitTreasury,
  runInitTreasuryWithFunds,
) where

import Plutus.Model (Run, adaValue, getMainUser, payToScript, spend, submitTx, userSpend)
import Plutus.Model.V2 (DatumMode (InlineDatum))
import PlutusTx.Prelude (Bool (True))
import Spec.SpecUtils (runInitPayToScript)
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Values (dummyTreasuryValue)
import Prelude (($), (<>))

import LambdaBuffers.ApplicationTypes.Treasury (TreasuryDatum (TreasuryDatum))

runInitTreasury :: Run ()
runInitTreasury =
  runInitPayToScript
    treasuryTypedValidator
    (TreasuryDatum True)
    dummyTreasuryValue

-- Initialize treasury with actual funds (4M ADA for disbursements + minAda)
-- Note: The treasury token is already in initialFunds, so we need to spend it separately
runInitTreasuryWithFunds :: Run ()
runInitTreasuryWithFunds = do
  admin <- getMainUser
  let value = adaValue 4_000_002 <> dummyTreasuryValue
  spendAda <- spend admin (adaValue 4_000_002)
  spendToken <- spend admin dummyTreasuryValue
  let payTx = payToScript treasuryTypedValidator (InlineDatum (TreasuryDatum True)) value
  submitTx admin $ payTx <> userSpend spendAda <> userSpend spendToken
