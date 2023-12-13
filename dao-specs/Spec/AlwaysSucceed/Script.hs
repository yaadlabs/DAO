module Spec.AlwaysSucceed.Script (
  alwaysSucceedTypedMintingPolicy,
  alwaysSucceedCurrencySymbol,
) where

import Plutus.Model.V2 (
  TypedPolicy,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx (compile)
import PlutusTx.Prelude (Bool (True), BuiltinData)

-- For upgrade minter testing
type AlwaysSucceedUpgradeMinter = TypedPolicy ()

alwaysSucceedTypedMintingPolicy :: AlwaysSucceedUpgradeMinter
alwaysSucceedTypedMintingPolicy =
  mkTypedPolicy $$(PlutusTx.compile [||toBuiltinPolicy succeedPolicy||])

succeedPolicy :: BuiltinData -> ScriptContext -> Bool
succeedPolicy _ _ = True

alwaysSucceedCurrencySymbol :: CurrencySymbol
alwaysSucceedCurrencySymbol = scriptCurrencySymbol alwaysSucceedTypedMintingPolicy
