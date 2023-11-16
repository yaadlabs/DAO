module Spec.Treasury.Utils (findTreasury) where

import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Values (
  dummyTreasurySymbol,
  dummyTreasuryTokenName,
 )

findTreasury :: Run (TxOutRef, TxOut, ())
findTreasury =
  findConfigUtxo
    treasuryTypedValidator
    dummyTreasurySymbol
    dummyTreasuryTokenName
