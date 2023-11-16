module Spec.AlwaysSucceed.Script (
  AlwaysSucceedScriptVoteDynamicConfig,
  AlwaysSucceedScriptTallyDynamicConfig,
  alwaysSucceedTypedValidator1,
  alwaysSucceedTypedValidator2,
  alwaysSucceedTypedMintingPolicy,
  alwaysSucceedCurrencySymbol,
) where

import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  mkTypedValidator,
  scriptCurrencySymbol,
  toBuiltinPolicy,
  toBuiltinValidator,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx (compile)
import PlutusTx.Prelude (Bool (True), BuiltinData)
import Triphut.AlwaysSucceed (succeedValidator1)
import Triphut.Tally (TallyDynamicConfigDatum)
import Triphut.Vote (VoteMinterDynamicConfigDatum)

-- Vote dynamic config reference script
type AlwaysSucceedScriptVoteDynamicConfig = TypedValidator VoteMinterDynamicConfigDatum ()

alwaysSucceedTypedValidator1 :: AlwaysSucceedScriptVoteDynamicConfig
alwaysSucceedTypedValidator1 =
  mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator1||])

-- Tally dynamic config reference script
type AlwaysSucceedScriptTallyDynamicConfig = TypedValidator TallyDynamicConfigDatum ()

alwaysSucceedTypedValidator2 :: AlwaysSucceedScriptTallyDynamicConfig
alwaysSucceedTypedValidator2 =
  mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator1||])

-- For upgrade minter testing
type AlwaysSucceedUpgradeMinter = TypedPolicy ()

alwaysSucceedTypedMintingPolicy :: AlwaysSucceedUpgradeMinter
alwaysSucceedTypedMintingPolicy =
  mkTypedPolicy $$(PlutusTx.compile [||toBuiltinPolicy succeedPolicy||])

succeedPolicy :: BuiltinData -> ScriptContext -> Bool
succeedPolicy _ _ = True

alwaysSucceedCurrencySymbol :: CurrencySymbol
alwaysSucceedCurrencySymbol = scriptCurrencySymbol alwaysSucceedTypedMintingPolicy
