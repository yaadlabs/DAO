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
import PlutusLedgerApi.V1.Value (singleton)
import Spec.SampleData (sampleTallyPolicyParams)
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
import Spec.Tally.Script (tallyConfigNftCurrencySymbol, tallyNftTypedValidator)
import Spec.Values (dummyTallyTokenName)

runInitTripTallyWithEndTimeInFutureNotEnoughVotes :: Run ()
runInitTripTallyWithEndTimeInFutureNotEnoughVotes =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitTripTallyWithEndTimeInPastNotEnoughVotes :: Run ()
runInitTripTallyWithEndTimeInPastNotEnoughVotes =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes :: Run ()
runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitGeneralTallyWithEndTimeInFuture :: Run ()
runInitGeneralTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleGeneralWithEndTimeInFutureTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitGeneralTallyWithEndTimeInPast :: Run ()
runInitGeneralTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleGeneralWithEndTimeInPastTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitTripTallyWithEndTimeInFuture :: Run ()
runInitTripTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripWithEndTimeInFutureTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitTripTallyWithEndTimeInPast :: Run ()
runInitTripTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleTripWithEndTimeInPastTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitUpgradeTallyWithEndTimeInPast :: Run ()
runInitUpgradeTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithVotesEndTimeInPastTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum :: Run ()
runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitTallyWithEndTimeInPast :: Run ()
runInitTallyWithEndTimeInPast =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithEndTimeInPastTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)

runInitTallyWithEndTimeInFuture :: Run ()
runInitTallyWithEndTimeInFuture =
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithEndTimeInFutureTallyStateDatum
    (singleton (tallyConfigNftCurrencySymbol sampleTallyPolicyParams) dummyTallyTokenName 1)
