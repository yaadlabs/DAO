module Spec.AlwaysSucceed.Script (
  alwaysSucceedTypedValidator,
  alwaysSucceedTypedValidator1,
) where

import Plutus.Model.V2 (TypedValidator, mkTypedValidator)
import PlutusTx (compile)
import Triphut.AlwaysSucceed (succeedWrapped, succeedWrapped1)

alwaysSucceedTypedValidator :: TypedValidator () ()
alwaysSucceedTypedValidator = mkTypedValidator $$(PlutusTx.compile [||succeedWrapped||])

alwaysSucceedTypedValidator1 :: TypedValidator () ()
alwaysSucceedTypedValidator1 = mkTypedValidator $$(PlutusTx.compile [||succeedWrapped1||])
