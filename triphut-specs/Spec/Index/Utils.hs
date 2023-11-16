module Spec.Index.Utils (findIndex) where

import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.Index.Script (indexNftTypedValidator)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyIndexConfigNftSymbol, dummyIndexConfigNftTokenName)
import Triphut.Index (IndexNftDatum)

findIndex :: Run (TxOutRef, TxOut, IndexNftDatum)
findIndex =
  findConfigUtxo
    indexNftTypedValidator
    dummyIndexConfigNftSymbol
    dummyIndexConfigNftTokenName
