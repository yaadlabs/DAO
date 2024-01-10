module Spec.Tally.Transactions (
  runInitTallyWithEndTimeInPast,
  runInitTallyWithEndTimeInFuture,
  runInitTripTallyWithEndTimeInFuture,
  runInitUpgradeTallyWithEndTimeInPast,
  runInitGeneralTallyWithEndTimeInFuture,
  runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum,
  runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes,
  runInitTripTallyWithEndTimeInFutureNotEnoughVotes,
) where

import Plutus.Model (Run)
import Spec.SpecUtils (runInitPayToScript)
import Spec.Tally.SampleData (
  sampleGeneralWithEndTimeInFutureTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
 )
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyTallyValue)

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