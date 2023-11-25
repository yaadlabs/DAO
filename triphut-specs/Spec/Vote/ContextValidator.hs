{- |
Module      : Spec.Vote.ContextValidator
Description : Tests the vote validator and tally validator in on transaction
-}
module Spec.Vote.ContextValidator (
  -- * Count redeemer tests
  validVoteValidatorCountRedeemerTest,
  invalidVoteValidatorNoConfigInRefInputsTest,
  invalidVoteValidatorNoTallyInInputsTest,
  invalidVoteValidatorNoTallyConfigInRefInputsTest,
  invalidVoteValidatorNoVoteInInputsTest,
  invalidVoteStillInTallyPeriodTest,

  -- * Cancel redeemer tests
  validVoteValidatorCancelRedeemerTest,
  invalidNotSignedByOwnerVoteValidatorCancelRedeemerTest,
) where

import Control.Monad (when)
import Plutus.Model (
  Run,
  adaValue,
  currentTime,
  newUser,
  spend,
  spendScript,
  submitTx,
  userSpend,
  validateIn,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToScript,
  refInputInline,
 )
import Plutus.V1.Ledger.Interval (from)
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Tally.Transactions (
  runInitTallyConfig,
  runInitTallyWithEndTimeInFuture,
  runInitTallyWithEndTimeInPast,
 )
import Spec.Tally.Utils (findTally, findTallyConfig)
import Spec.Values (dummyTallyValue, dummyVoteValue)
import Spec.Vote.Script (voteTypedValidator)
import Spec.Vote.Transactions (runInitVote, runInitVoteWithUser)
import Spec.Vote.Utils (findVote)
import Triphut.Vote (VoteActionRedeemer (Cancel, Count))
import Prelude (Eq, mconcat, mempty, ($), (<>), (==))

-- | Count redeemer tests
validVoteValidatorCountRedeemerTest :: Run ()
validVoteValidatorCountRedeemerTest =
  mkVoteValidatorCountRedeemerTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver

invalidVoteValidatorNoConfigInRefInputsTest :: Run ()
invalidVoteValidatorNoConfigInRefInputsTest =
  mkVoteValidatorCountRedeemerTest
    NoConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver

invalidVoteValidatorNoTallyInInputsTest :: Run ()
invalidVoteValidatorNoTallyInInputsTest =
  mkVoteValidatorCountRedeemerTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    NoTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver

invalidVoteValidatorNoVoteInInputsTest :: Run ()
invalidVoteValidatorNoVoteInInputsTest =
  mkVoteValidatorCountRedeemerTest
    HasConfigInReferenceInputs
    NoVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver

invalidVoteValidatorNoTallyConfigInRefInputsTest :: Run ()
invalidVoteValidatorNoTallyConfigInRefInputsTest =
  mkVoteValidatorCountRedeemerTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    NoTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver

invalidVoteStillInTallyPeriodTest :: Run ()
invalidVoteStillInTallyPeriodTest =
  mkVoteValidatorCountRedeemerTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    StillInTallyPeriod

data VoteConfigReference
  = HasConfigInReferenceInputs
  | NoConfigInReferenceInputs
  deriving stock (Eq)

data TallyValidatorInput
  = HasTallyValidatorInInputs
  | NoTallyValidatorInInputs
  deriving stock (Eq)

data VoteValidatorInput
  = HasVoteValidatorInInputs
  | NoVoteValidatorInInputs
  deriving stock (Eq)

data TallyConfigReference
  = HasTallyConfigInReferenceInputs
  | NoTallyConfigInReferenceInputs
  deriving stock (Eq)

data TallyPeriod
  = TallyPeriodOver
  | StillInTallyPeriod
  deriving stock (Eq)

mkVoteValidatorCountRedeemerTest ::
  VoteConfigReference ->
  VoteValidatorInput ->
  TallyValidatorInput ->
  TallyConfigReference ->
  TallyPeriod ->
  Run ()
mkVoteValidatorCountRedeemerTest configRef voteValidator tallyValidator tallyConfigRef tallyPeriod = do
  runInitVote
  runInitConfig
  runInitTallyConfig

  when (tallyPeriod == TallyPeriodOver) runInitTallyWithEndTimeInPast -- Valid
  when (tallyPeriod == StillInTallyPeriod) runInitTallyWithEndTimeInFuture -- Invalid
  (configOutRef, _, _) <- findConfig
  (tallyConfigOutRef, _, _) <- findTallyConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (voteOutRef, _, voteDatum) <- findVote

  user <- newUser $ amountOfAda 4_000_000
  spend1 <- spend user $ amountOfAda 2_000_000
  spend2 <- spend user $ amountOfAda 2_000_002

  theTimeNow <- currentTime

  let baseTx =
        mconcat
          [ userSpend spend1
          , userSpend spend2
          ]

      withVoteConfigRef = case configRef of
        HasConfigInReferenceInputs -> refInputInline configOutRef
        NoConfigInReferenceInputs -> mempty

      withTallyRef = case tallyConfigRef of
        HasTallyConfigInReferenceInputs -> refInputInline tallyConfigOutRef
        NoTallyConfigInReferenceInputs -> mempty

      withVoteValidator = case voteValidator of
        HasVoteValidatorInInputs -> spendScript voteTypedValidator voteOutRef Count voteDatum
        NoVoteValidatorInInputs -> mempty

      withTallyValidator = case tallyValidator of
        HasTallyValidatorInInputs -> spendScript tallyNftTypedValidator tallyOutRef () tallyDatum
        NoTallyValidatorInInputs -> mempty

      payToTallyValidator =
        payToScript
          tallyNftTypedValidator
          (InlineDatum tallyDatum)
          (amountOfAda 4_000_000 <> dummyTallyValue)

      payToVoteValidator =
        payToScript
          voteTypedValidator
          (InlineDatum voteDatum)
          (adaValue 2 <> dummyVoteValue)

      combinedTxs =
        mconcat
          [ baseTx
          , withVoteConfigRef
          , withVoteValidator
          , withTallyValidator
          , withTallyRef
          , payToTallyValidator
          , payToVoteValidator
          ]

  -- We want to ensure the `tsProposalEndTime` is before the valid range
  -- Hence using the `from` function here and setting `tsProposalEndTime` to zero
  -- in the sample tally datum
  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user $ finalTx

-- | Cancel redeemer tests
validVoteValidatorCancelRedeemerTest :: Run ()
validVoteValidatorCancelRedeemerTest =
  mkVoteValidatorCancelRedeemerTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver
    OwnerSignature

invalidNotSignedByOwnerVoteValidatorCancelRedeemerTest :: Run ()
invalidNotSignedByOwnerVoteValidatorCancelRedeemerTest =
  mkVoteValidatorCancelRedeemerTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs
    TallyPeriodOver
    NoOwnerSignature

data VoteOwnerSignsTx
  = OwnerSignature
  | NoOwnerSignature
  deriving stock (Eq)

mkVoteValidatorCancelRedeemerTest ::
  VoteConfigReference ->
  VoteValidatorInput ->
  TallyValidatorInput ->
  TallyConfigReference ->
  TallyPeriod ->
  VoteOwnerSignsTx ->
  Run ()
mkVoteValidatorCancelRedeemerTest
  configRef
  voteValidator
  tallyValidator
  tallyConfigRef
  tallyPeriod
  ownerSigns = do
    runInitConfig
    runInitTallyConfig

    when (tallyPeriod == TallyPeriodOver) runInitTallyWithEndTimeInPast -- Valid
    when (tallyPeriod == StillInTallyPeriod) runInitTallyWithEndTimeInFuture -- Invalid
    (configOutRef, _, _) <- findConfig
    (tallyConfigOutRef, _, _) <- findTallyConfig
    (tallyOutRef, _, tallyDatum) <- findTally

    user <- newUser $ amountOfAda 4_000_000
    spend1 <- spend user $ amountOfAda 2_000_000
    spend2 <- spend user $ amountOfAda 2_000_002

    -- For positive test we want the `vOwner` field of the `VoteDatum`
    -- to be equal to the `user` signing the transaction
    when (ownerSigns == OwnerSignature) $ runInitVoteWithUser user
    when (ownerSigns == NoOwnerSignature) $ runInitVote
    (voteOutRef, _, voteDatum) <- findVote

    theTimeNow <- currentTime

    let baseTx =
          mconcat
            [ userSpend spend1
            , userSpend spend2
            ]

        withVoteConfigRef = case configRef of
          HasConfigInReferenceInputs -> refInputInline configOutRef
          NoConfigInReferenceInputs -> mempty

        withTallyRef = case tallyConfigRef of
          HasTallyConfigInReferenceInputs -> refInputInline tallyConfigOutRef
          NoTallyConfigInReferenceInputs -> mempty

        withVoteValidator = case voteValidator of
          HasVoteValidatorInInputs -> spendScript voteTypedValidator voteOutRef Cancel voteDatum
          NoVoteValidatorInInputs -> mempty

        withTallyValidator = case tallyValidator of
          HasTallyValidatorInInputs -> spendScript tallyNftTypedValidator tallyOutRef () tallyDatum
          NoTallyValidatorInInputs -> mempty

        payToTallyValidator =
          payToScript
            tallyNftTypedValidator
            (InlineDatum tallyDatum)
            (amountOfAda 4_000_000 <> dummyTallyValue)

        payToVoteValidator =
          payToScript
            voteTypedValidator
            (InlineDatum voteDatum)
            (adaValue 2 <> dummyVoteValue)

        combinedTxs =
          mconcat
            [ baseTx
            , withVoteConfigRef
            , withVoteValidator
            , withTallyValidator
            , withTallyRef
            , payToTallyValidator
            , payToVoteValidator
            ]

    -- We want to ensure the `tsProposalEndTime` is before the valid range
    -- Hence using the `from` function here and setting `tsProposalEndTime` to zero
    -- in the sample tally datum
    finalTx <- validateIn (from theTimeNow) combinedTxs

    submitTx user finalTx
