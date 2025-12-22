module Spec.Treasury.Utils (findTreasury) where

import LambdaBuffers.ApplicationTypes.Treasury (TreasuryDatum)
import Plutus.Model (Run)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Values (
  dummyTreasurySymbol,
  dummyTreasuryTokenName,
 )

findTreasury :: Run (TxOutRef, TxOut, TreasuryDatum)
findTreasury =
  findConfigUtxo
    treasuryTypedValidator
    dummyTreasurySymbol
    dummyTreasuryTokenName
