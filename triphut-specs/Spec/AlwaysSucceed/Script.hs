module Spec.AlwaysSucceed.Script (
  alwaysSucceedTypedValidator,
) where

import Plutus.Model.V2 (TypedValidator, mkTypedValidator, toBuiltinValidator)
import PlutusTx (compile)
import Triphut.AlwaysSucceed (succeedValidator)
import Triphut.Types (DynamicConfig)

alwaysSucceedTypedValidator :: TypedValidator DynamicConfig ()
alwaysSucceedTypedValidator = mkTypedValidator $$(PlutusTx.compile [||toBuiltinValidator succeedValidator||])
