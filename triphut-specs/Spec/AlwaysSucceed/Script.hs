module Spec.AlwaysSucceed.Script (
  AlwaysSucceedScriptVoteMinterDynamicConfig,
  AlwaysSucceedScriptTallyDynamicConfig,
  alwaysSucceedTypedValidator1,
  alwaysSucceedTypedValidator2,
  alwaysSucceedTypedValidator3,
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
import Triphut.Vote (VoteDynamicConfigDatum, VoteMinterDynamicConfigDatum)

-- Vote dynamic config reference script
type AlwaysSucceedScriptVoteMinterDynamicConfig = TypedValidator VoteMinterDynamicConfigDatum ()

alwaysSucceedTypedValidator1 :: AlwaysSucceedScriptVoteMinterDynamicConfig
alwaysSucceedTypedValidator1 =
  mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator1||])

-- Tally dynamic config reference script
type AlwaysSucceedScriptTallyDynamicConfig = TypedValidator TallyDynamicConfigDatum ()

alwaysSucceedTypedValidator2 :: AlwaysSucceedScriptTallyDynamicConfig
alwaysSucceedTypedValidator2 =
  mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator1||])

-- Vote dynamic config reference script
type AlwaysSucceedScriptVoteDynamicConfig = TypedValidator VoteDynamicConfigDatum ()

alwaysSucceedTypedValidator3 :: AlwaysSucceedScriptVoteDynamicConfig
alwaysSucceedTypedValidator3 =
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
