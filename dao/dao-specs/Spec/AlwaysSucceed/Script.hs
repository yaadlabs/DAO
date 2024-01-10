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
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusLedgerApi.V2.Contexts (ScriptContext)
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
