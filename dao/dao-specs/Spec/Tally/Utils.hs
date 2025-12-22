module Spec.Tally.Utils (findTally) where

import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Plutus.Model (Run)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
import Spec.SampleData (sampleTallyPolicyParams)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Tally.Script (tallyConfigNftCurrencySymbol, tallyNftTypedValidator)
import Spec.Values (dummyTallyTokenName)

findTally :: Run (TxOutRef, TxOut, TallyStateDatum)
findTally =
  findConfigUtxo
    tallyNftTypedValidator
    (tallyConfigNftCurrencySymbol sampleTallyPolicyParams)
    dummyTallyTokenName
