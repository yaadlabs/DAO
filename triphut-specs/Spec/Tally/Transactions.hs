module Spec.Tally.Transactions (
  runInitTallyWithEndTimeInPast,
  runInitTallyWithEndTimeInFuture,
  runInitTripTallyWithEndTimeInFuture,
  runInitUpgradeTallyWithEndTimeInPast,
  runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum,
  runInitGeneralTallyWithEndTimeInFuture,
  runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum,
  runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes,
  runInitTripTallyWithEndTimeInFutureNotEnoughVotes,
  runInitTallyConfig,
) where

import Plutus.Model (Run)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator2)
import Spec.SampleData (sampleTallyDynamicConfig)
import Spec.SpecUtils (minAda, runInitPayToScript, runInitReferenceScript)
import Spec.Tally.SampleData (
  sampleGeneralWithEndTimeInFutureTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
 )
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyTallyConfigValue, dummyTallyValue)
import Prelude ((<>))

runInitTripTallyWithEndTimeInFutureNotEnoughVotes :: Run ()
runInitTripTallyWithEndTimeInFutureNotEnoughVotes =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes :: Run ()
runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum
    dummyTallyValue

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
