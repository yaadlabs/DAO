module Spec.Tally.Utils (findTally) where

import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (
  dummyTallySymbol,
  dummyTallyTokenName,
 )
import Triphut.Types (TallyStateDatum)

findTally :: Run (TxOutRef, TxOut, TallyStateDatum)
findTally =
  findConfigUtxo
    tallyNftTypedValidator
    dummyTallySymbol
    dummyTallyTokenName
