{- |
Module      : Spec.Vote.ContextValidator
Description : Tests the vote validator and tally validator in on transaction
-}
module Spec.Vote.ContextValidator (
  validVoteValidatorTest,
  invalidVoteValidatorNoConfigInRefInputsTest,
  invalidVoteValidatorNoTallyInInputsTest,
  invalidVoteValidatorNoTallyConfigInRefInputsTest,
  invalidVoteValidatorNoVoteInInputsTest,
) where

import Control.Monad (void)
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
  logInfo,
  payToScript,
  refInputInline,
 )
import Plutus.V1.Ledger.Interval (from)
import PlutusTx.Prelude (($))
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Tally.Transactions (runInitTallyConfig, runInitTallyWithEndTimeInPast)
import Spec.Tally.Utils (findTally, findTallyConfig)
import Spec.Values (dummyTallyValue, dummyVoteValue)
import Spec.Vote.Script (voteTypedValidator)
import Spec.Vote.Transactions (runInitVote, runInitVoteConfig)
import Spec.Vote.Utils (findVote, findVoteConfig)
import Triphut.Vote (VoteActionRedeemer (Count))
import Prelude (Eq, mconcat, mempty, pure, show, (*), (+), (<>))

validVoteValidatorTest :: Run ()
validVoteValidatorTest =
  mkVoteValidatorTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs

invalidVoteValidatorNoConfigInRefInputsTest :: Run ()
invalidVoteValidatorNoConfigInRefInputsTest =
  mkVoteValidatorTest
    NoConfigInReferenceInputs
    HasVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs

invalidVoteValidatorNoTallyInInputsTest :: Run ()
invalidVoteValidatorNoTallyInInputsTest =
  mkVoteValidatorTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    NoTallyValidatorInInputs
    HasTallyConfigInReferenceInputs

invalidVoteValidatorNoVoteInInputsTest :: Run ()
invalidVoteValidatorNoVoteInInputsTest =
  mkVoteValidatorTest
    HasConfigInReferenceInputs
    NoVoteValidatorInInputs
    HasTallyValidatorInInputs
    HasTallyConfigInReferenceInputs

invalidVoteValidatorNoTallyConfigInRefInputsTest :: Run ()
invalidVoteValidatorNoTallyConfigInRefInputsTest =
  mkVoteValidatorTest
    HasConfigInReferenceInputs
    HasVoteValidatorInInputs
    NoTallyValidatorInInputs
    HasTallyConfigInReferenceInputs

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

mkVoteValidatorTest ::
  VoteConfigReference ->
  VoteValidatorInput ->
  TallyValidatorInput ->
  TallyConfigReference ->
  Run ()
mkVoteValidatorTest configRef voteValidator tallyValidator tallyConfigRef = do
  runInitVoteConfig
  runInitTallyConfig
  runInitTallyWithEndTimeInPast
  runInitVote

  (voteConfigOutRef, _, _) <- findVoteConfig
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
        HasConfigInReferenceInputs -> refInputInline voteConfigOutRef
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
