module Spec.Tally.Transactions (
  runInitTallyWithEndTimeInPast,
  runInitTallyWithEndTimeInFuture,
  runInitTripTallyWithEndTimeInFuture,
  runInitUpgradeTallyWithEndTimeInPast,
  runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum,
  runInitGeneralTallyWithEndTimeInFuture,
  runInitTallyConfig,
) where

import Plutus.Model (Run)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator2)
import Spec.SampleData (sampleTallyDynamicConfig)
import Spec.SpecUtils (minAda, runInitPayToScript, runInitReferenceScript)
import Spec.Tally.SampleData (
  sampleGeneralWithEndTimeInFutureTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
 )
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyTallyConfigValue, dummyTallyValue)
import Prelude ((<>))

runInitGeneralTallyWithEndTimeInFuture :: Run ()
runInitGeneralTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleGeneralWithEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitTripTallyWithEndTimeInFuture :: Run ()
runInitTripTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripWithEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitUpgradeTallyWithEndTimeInPast :: Run ()
runInitUpgradeTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithVotesEndTimeInPastTallyStateDatum
    dummyTallyValue

runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum :: Run ()
runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitTallyWithEndTimeInPast :: Run ()
runInitTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithEndTimeInPastTallyStateDatum
    dummyTallyValue

runInitTallyWithEndTimeInFuture :: Run ()
runInitTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitTallyConfig :: Run ()
runInitTallyConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator2
    sampleTallyDynamicConfig
    (dummyTallyConfigValue <> minAda)
