module Spec.Configuration.Utils (findConfig) where

import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import Plutus.Model (Run)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
import Spec.Configuration.Script (upgradeConfigNftTypedValidator)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)

findConfig :: Run (TxOutRef, TxOut, DynamicConfigDatum)
findConfig =
  findConfigUtxo
    upgradeConfigNftTypedValidator
    dummyConfigNftSymbol
    dummyConfigNftTokenName
