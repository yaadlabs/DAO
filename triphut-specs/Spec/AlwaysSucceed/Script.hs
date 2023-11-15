module Spec.AlwaysSucceed.Script (
  AlwaysSucceedScriptDynamicConfig,
  AlwaysSucceedScriptVoteDynamicConfig,
  AlwaysSucceedScriptTallyDynamicConfig,
  alwaysSucceedTypedValidator,
  alwaysSucceedTypedValidator1,
  alwaysSucceedTypedValidator2,
) where

import Plutus.Model.V2 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import PlutusTx (compile)
import Triphut.AlwaysSucceed (succeedValidator, succeedValidator1)
import Triphut.Tally (TallyDynamicConfigDatum)
import Triphut.Types (DynamicConfigDatum)
import Triphut.Vote (VoteMinterDynamicConfigDatum)

-- Dynamic config reference script
type AlwaysSucceedScriptDynamicConfig = TypedValidator DynamicConfigDatum ()

alwaysSucceedTypedValidator :: AlwaysSucceedScriptDynamicConfig
alwaysSucceedTypedValidator =
  mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator||])

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
