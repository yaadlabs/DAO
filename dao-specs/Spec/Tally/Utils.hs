module Spec.Tally.Utils (findTally, findTallyConfig) where

import Dao.Tally (TallyDynamicConfigDatum)
import Dao.Types (TallyStateDatum)
import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator2)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (
  dummyTallyConfigSymbol,
  dummyTallyConfigTokenName,
  dummyTallySymbol,
  dummyTallyTokenName,
 )

findTally :: Run (TxOutRef, TxOut, TallyStateDatum)
findTally =
  findConfigUtxo
    tallyNftTypedValidator
    dummyTallySymbol
    dummyTallyTokenName

findTallyConfig :: Run (TxOutRef, TxOut, TallyDynamicConfigDatum)
findTallyConfig =
  findConfigUtxo
    alwaysSucceedTypedValidator2
    dummyTallyConfigSymbol
    dummyTallyConfigTokenName
