module Spec.Tally.Utils (findTally) where

import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Plutus.Model (Run)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (
  dummyTallySymbol,
  dummyTallyTokenName,
 )

findTally :: Run (TxOutRef, TxOut, TallyStateDatum)
findTally =
  findConfigUtxo
    tallyNftTypedValidator
    dummyTallySymbol
    dummyTallyTokenName
