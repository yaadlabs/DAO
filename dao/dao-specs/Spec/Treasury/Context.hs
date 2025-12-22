{- |
Module      : Spec.Treasury.Context
Description : Treasury validator unit tests
-}
module Spec.Treasury.Context (
  validTripTreasuryTest,
  validUpgradeTreasuryTest,
  validGeneralTreasuryTest,
  invalidNotEnoughVotesTripTreasuryTest,
) where

import Control.Monad (when)
import LambdaBuffers.ApplicationTypes.Treasury (TreasuryDatum (TreasuryDatum))
import Plutus.Model (
  Run,
  adaValue,
  currentTime,
  mintValue,
  newUser,
  spend,
  spendScript,
  submitTx,
  userSpend,
  validateIn,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToKey,
  payToScript,
  refInputInline,
 )
import PlutusLedgerApi.V1.Interval (from)
import PlutusLedgerApi.V1.Value (Value, adaToken, singleton)
import PlutusTx.Prelude (Bool (True), ($))
import Spec.Addresses (
  dummyGeneralPaymentAddress,
  dummyTravelerPaymentAddress,
 )
import Spec.AlwaysSucceed.Script (
  alwaysSucceedCurrencySymbol,
  alwaysSucceedTypedMintingPolicy,
 )
import Spec.Configuration.Transactions (
  runHighRelativeMajorityTotalVotesInitConfig,
  runInitConfig,
  runInitHighThresholdTreasuryTestConfig,
  runInitTreasuryTestConfig,
 )
import Spec.Configuration.Utils (findConfig)
import Spec.Index.Transactions (runInitIndex)
import Spec.SampleData (sampleTallyPolicyParams)
import Spec.SpecUtils (amountOfAda, findConfigUtxo, runInitPayToScript)
import Spec.Tally.SampleData (
  sampleGeneralWithEndTimeInPastTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleTripWithEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
 )
import Spec.Tally.Script (
  tallyConfigNftCurrencySymbol,
  tallyConfigNftTypedMintingPolicy,
  tallyNftTypedValidator,
 )
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Treasury.Transactions (runInitTreasury, runInitTreasuryWithFunds)
import Spec.Treasury.Utils (findTreasury)
import Spec.Values (dummyTallySymbol, dummyTallyTokenName, dummyTallyValue, dummyTreasurySymbol, dummyTreasuryTokenName, dummyTreasuryValue)
import Spec.Vote.Transactions (runInitVoteNft)
import Prelude (Eq, mconcat, (<>), (==))

-- Positive test for when the proposal is a Trip proposal
validTripTreasuryTest :: Run ()
validTripTreasuryTest = mkTripTreasuryTest HasEnoughVotes

invalidNotEnoughVotesTripTreasuryTest :: Run ()
invalidNotEnoughVotesTripTreasuryTest = mkTripTreasuryTest NotEnoughVotes

data EnoughVotes
  = HasEnoughVotes -- Valid
  | NotEnoughVotes -- Invalid
  deriving stock (Eq)

mkTripTreasuryTest :: EnoughVotes -> Run ()
mkTripTreasuryTest enoughVotes = do
  -- Choose which config to load based on whether we want to trigger
  -- the negative test for not enough votes or not
  -- Use special Treasury test config that has alwaysSucceed for tally burning (ID-501)
  when (enoughVotes == HasEnoughVotes) runInitTreasuryTestConfig
  when (enoughVotes == NotEnoughVotes) runInitHighThresholdTreasuryTestConfig

  runInitIndex
  runInitVoteNft

  -- Use alwaysSucceedCurrencySymbol for tally (so we can burn it per ID-501)
  -- Note: Original tests used dummyTallyValue but didn't burn. After audit fix ID-501, we MUST burn the tally.
  let tallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName 1
  when (enoughVotes == HasEnoughVotes) $
    runInitPayToScript tallyNftTypedValidator sampleTripWithEndTimeInPastTallyStateDatum tallyValue
  when (enoughVotes == NotEnoughVotes) $
    runInitPayToScript tallyNftTypedValidator sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum tallyValue

  runInitTreasury -- Treasury starts with just minAda + treasury token (like original)
  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findConfigUtxo tallyNftTypedValidator alwaysSucceedCurrencySymbol dummyTallyTokenName
  (treasuryOutRef, _, treasuryDatum) <- findConfigUtxo treasuryTypedValidator dummyTreasurySymbol dummyTreasuryTokenName

  user <- newUser $ amountOfAda 3_000_000

  let
    -- ID-501: Burn the tally token (was referenced in original, now must be spent and burned)
    burnDummyTallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName (-1)

    baseTx =
      mconcat
        [ spendScript treasuryTypedValidator treasuryOutRef () (TreasuryDatum True)
        , refInputInline configOutRef
        , spendScript tallyNftTypedValidator tallyOutRef () tallyDatum -- ID-501: spend instead of reference
        , mintValue alwaysSucceedTypedMintingPolicy () burnDummyTallyValue -- ID-501: burn tally
        ]

    -- ID-401: With disbursedAmount=1 lovelace, Treasury remainder (2M - 1) is below 3M threshold
    -- Must send remainder (including treasury token) to fallback address
    -- Treasury 2M + Tally 2M = 4M total from scripts, minus 2 ADA disbursed = 3_999_998
    payToFallback = payToKey dummyGeneralPaymentAddress (amountOfAda 3_999_998 <> dummyTreasuryValue)

    -- Need to pay 2 ADA to the traveller's payment address provided (from disbursement)
    payToTravelerAddress = payToKey dummyTravelerPaymentAddress (adaValue 2)

    combinedTxs = baseTx <> payToFallback <> payToTravelerAddress

  theTimeNow <- currentTime
  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx

-- Positive test for when the proposal is an Upgrade proposal
validUpgradeTreasuryTest :: Run ()
validUpgradeTreasuryTest = do
  runInitTreasuryTestConfig -- Use special Treasury test config for tally burning (ID-501)
  runInitIndex
  runInitVoteNft

  -- Use alwaysSucceedCurrencySymbol for tally (so we can burn it per ID-501)
  -- Use the one with votes to avoid division by zero
  let tallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName 1
  runInitPayToScript tallyNftTypedValidator sampleUpgradeWithVotesEndTimeInPastTallyStateDatum tallyValue

  runInitTreasury -- Treasury starts with just minAda + treasury token (like original)
  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findConfigUtxo tallyNftTypedValidator alwaysSucceedCurrencySymbol dummyTallyTokenName
  (treasuryOutRef, _, treasuryDatum) <- findConfigUtxo treasuryTypedValidator dummyTreasurySymbol dummyTreasuryTokenName

  user <- newUser $ amountOfAda 3_000_000

  theTimeNow <- currentTime

  let
    -- ID-501 & ID-503: Burn the tally token (replaces separate upgrade token per ID-503)
    burnDummyTallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName (-1)

    baseTx =
      mconcat
        [ spendScript treasuryTypedValidator treasuryOutRef () (TreasuryDatum True)
        , refInputInline configOutRef
        , spendScript tallyNftTypedValidator tallyOutRef () tallyDatum -- ID-501: spend instead of reference
        , mintValue alwaysSucceedTypedMintingPolicy () burnDummyTallyValue -- ID-501 & ID-503: burn tally
        ]

    -- ID-502: Upgrade proposal - ALL funds must go to new treasury address (no continuing output)
    -- Treasury 2M + Tally 2M = 4M + treasury token, all sent to new treasury address
    payToNewTreasuryAddress = payToKey dummyGeneralPaymentAddress (amountOfAda 4_000_000 <> dummyTreasuryValue)

    combinedTxs = baseTx <> payToNewTreasuryAddress

  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx

-- Positive test for when the proposal is a General proposal
validGeneralTreasuryTest :: Run ()
validGeneralTreasuryTest = do
  runInitTreasuryTestConfig -- Use special Treasury test config for tally burning (ID-501)
  runInitIndex
  runInitVoteNft

  -- Use simplified dummy Tally initialization with alwaysSucceed token
  let tallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName 1
  runInitPayToScript tallyNftTypedValidator sampleGeneralWithEndTimeInPastTallyStateDatum tallyValue

  runInitTreasury -- Treasury starts with just minAda + treasury token (like original)
  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findConfigUtxo tallyNftTypedValidator alwaysSucceedCurrencySymbol dummyTallyTokenName
  (treasuryOutRef, _, treasuryDatum) <- findConfigUtxo treasuryTypedValidator dummyTreasurySymbol dummyTreasuryTokenName

  user <- newUser $ amountOfAda 3_000_000

  let
    -- ID-501: Burn the tally token (was referenced in original, now must be spent and burned)
    burnDummyTallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName (-1)

    baseTx =
      mconcat
        [ spendScript treasuryTypedValidator treasuryOutRef () (TreasuryDatum True)
        , refInputInline configOutRef
        , spendScript tallyNftTypedValidator tallyOutRef () tallyDatum -- ID-501: spend instead of reference
        , mintValue alwaysSucceedTypedMintingPolicy () burnDummyTallyValue -- ID-501: burn tally
        ]

    -- ID-401: With maxGeneralDisbursement=1 lovelace, Treasury remainder (2M - 1) is below 3M threshold
    -- Must send remainder (including treasury token) to fallback address
    -- Treasury 2M + Tally 2M = 4M total from scripts, minus 2 ADA disbursed = 3_999_998
    payToFallback = payToKey dummyGeneralPaymentAddress (amountOfAda 3_999_998 <> dummyTreasuryValue)

    -- Need to pay 2 ADA to the payment address provided (from disbursement)
    payToGeneralAddress = payToKey dummyGeneralPaymentAddress (adaValue 2)

    combinedTxs =
      mconcat
        [ baseTx
        , payToFallback
        , payToGeneralAddress
        ]

  theTimeNow <- currentTime
  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx
