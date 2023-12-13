module Spec.ConfigurationNft.Utils (findConfig) where

import Dao.Types (DynamicConfigDatum)
import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.ConfigurationNft.Script (upgradeConfigNftTypedValidator)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)

findConfig :: Run (TxOutRef, TxOut, DynamicConfigDatum)
findConfig =
  findConfigUtxo
    upgradeConfigNftTypedValidator
    dummyConfigNftSymbol
    dummyConfigNftTokenName