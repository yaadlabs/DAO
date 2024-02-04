module Spec.Index.Utils (findIndex) where

import LambdaBuffers.ApplicationTypes.Index (IndexDatum)
import Plutus.Model (Run)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
import Spec.Index.Script (indexTypedValidator)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyIndexConfigNftSymbol, dummyIndexConfigNftTokenName)

findIndex :: Run (TxOutRef, TxOut, IndexDatum)
findIndex =
  findConfigUtxo
    indexTypedValidator
    dummyIndexConfigNftSymbol
    dummyIndexConfigNftTokenName
