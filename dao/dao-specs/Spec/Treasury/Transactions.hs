module Spec.Treasury.Transactions (
  runInitTreasury,
) where

import Plutus.Model (Run)
import Spec.SpecUtils (runInitPayToScript)
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Values (dummyTreasuryValue)

runInitTreasury :: Run ()
runInitTreasury =
  runInitPayToScript
    treasuryTypedValidator
    ()
    dummyTreasuryValue
