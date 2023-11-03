module Spec.AlwaysSucceed.Script (
  alwaysSucceedTypedValidator,
  alwaysSucceedTypedValidator1,
  alwaysSucceedsValidator,
  alwaysSucceedsValidator1,
) where

import Plutus.Model.V2 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import Plutus.V1.Ledger.Scripts (Validator, mkValidatorScript)
import PlutusTx (compile)
import Triphut.AlwaysSucceed (succeedValidator, succeedValidator1)

alwaysSucceedTypedValidator :: TypedValidator () ()
alwaysSucceedTypedValidator = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator||])

alwaysSucceedTypedValidator1 :: TypedValidator () ()
alwaysSucceedTypedValidator1 = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator1||])

alwaysSucceedsValidator :: Validator
alwaysSucceedsValidator = mkValidatorScript $$(PlutusTx.compile [||toBuiltinValidator succeedValidator||])

alwaysSucceedsValidator1 :: Validator
alwaysSucceedsValidator1 = mkValidatorScript $$(PlutusTx.compile [||toBuiltinValidator succeedValidator1||])
