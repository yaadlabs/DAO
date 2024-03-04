module Spec.Tally.Transactions (
  runInitTallyWithEndTimeInPast,
  runInitTallyWithEndTimeInFuture,
  runInitTripTallyWithEndTimeInFuture,
  runInitTripTallyWithEndTimeInPastNotEnoughVotes,
  runInitUpgradeTallyWithEndTimeInPast,
  runInitGeneralTallyWithEndTimeInFuture,
  runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum,
  runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes,
  runInitTripTallyWithEndTimeInFutureNotEnoughVotes,
  runInitTripTallyWithEndTimeInPast,
  runInitGeneralTallyWithEndTimeInPast,
) where

import Plutus.Model (Run)
import Spec.SpecUtils (runInitPayToScript)
import Spec.Tally.SampleData (
  sampleGeneralWithEndTimeInFutureTallyStateDatum,
  sampleGeneralWithEndTimeInPastTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleTripWithEndTimeInPastTallyStateDatum,
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

runInitTripTallyWithEndTimeInPastNotEnoughVotes :: Run ()
runInitTripTallyWithEndTimeInPastNotEnoughVotes =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum
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

runInitGeneralTallyWithEndTimeInPast :: Run ()
runInitGeneralTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleGeneralWithEndTimeInPastTallyStateDatum
    dummyTallyValue

runInitTripTallyWithEndTimeInFuture :: Run ()
runInitTripTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripWithEndTimeInFutureTallyStateDatum
    dummyTallyValue

runInitTripTallyWithEndTimeInPast :: Run ()
runInitTripTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripWithEndTimeInPastTallyStateDatum
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
