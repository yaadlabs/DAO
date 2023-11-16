module Spec.ConfigurationNft.Utils (findConfig) where

import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.ConfigurationNft.Script (upgradeConfigNftTypedValidator)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)
import Triphut.Types (DynamicConfigDatum)

findConfig :: Run (TxOutRef, TxOut, DynamicConfigDatum)
findConfig =
  findConfigUtxo
    upgradeConfigNftTypedValidator
    dummyConfigNftSymbol
    dummyConfigNftTokenName
