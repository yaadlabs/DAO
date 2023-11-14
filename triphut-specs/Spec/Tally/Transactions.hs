module Spec.Tally.Transactions (runInitTally) where

import Plutus.Model (Run)
import Spec.SpecUtils (runInitPayToScript)
import Spec.Tally.SampleData (sampleTallyStateDatum)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyTallyValue)

runInitTally :: Run ()
runInitTally =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTallyStateDatum
    dummyTallyValue
