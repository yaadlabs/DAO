module Spec.Tally.Transactions (
  runInitTallyWithEndTimeInPast,
  runInitTallyWithEndTimeInFuture,
  runInitTallyConfig,
) where

import Plutus.Model (Run)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator2)
import Spec.SampleData (sampleTallyDynamicConfig)
import Spec.SpecUtils (minAda, runInitPayToScript, runInitReferenceScript)
import Spec.Tally.SampleData (
  sampleWithEndTimeInFutureTallyStateDatum,
  sampleWithEndTimeInPastTallyStateDatum,
 )
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyTallyConfigValue, dummyTallyValue)
import Prelude ((<>))

runInitTallyWithEndTimeInPast :: Run ()
runInitTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleWithEndTimeInPastTallyStateDatum
    dummyTallyValue

runInitTallyWithEndTimeInFuture :: Run ()
runInitTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleWithEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitTallyConfig :: Run ()
runInitTallyConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator2
    sampleTallyDynamicConfig
    (dummyTallyConfigValue <> minAda)
