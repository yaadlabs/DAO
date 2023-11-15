module Spec.Vote.ContextValidator (validVoteValidatorTest) where

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
import Prelude (mconcat, pure, show, (*), (+), (<>))

validVoteValidatorTest :: Run ()
validVoteValidatorTest = mkVoteValidatorTest

mkVoteValidatorTest :: Run ()
mkVoteValidatorTest = do
  runInitVoteConfig
  runInitTallyConfig
  runInitTallyWithEndTimeInPast
  runInitVote

  (voteConfigOutRef, _, _) <- findVoteConfig
  (tallyConfigOutRef, _, _) <- findTallyConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (voteOutRef, _, voteDatum) <- findVote

  user <- newUser $ amountOfAda 4_000_000
  spend1 <- spend user (adaValue 2)
  spend2 <- spend user (adaValue 2)

  theTimeNow <- currentTime

  let baseTx =
        mconcat
          [ spendScript tallyNftTypedValidator tallyOutRef () tallyDatum
          , spendScript voteTypedValidator voteOutRef Count voteDatum
          , refInputInline voteConfigOutRef
          , refInputInline tallyConfigOutRef
          , userSpend spend1
          , userSpend spend2
          ]

      payToTallyValidator =
        payToScript
          tallyNftTypedValidator
          (InlineDatum tallyDatum)
          (adaValue 2 <> dummyTallyValue)

      payToVoteValidator =
        payToScript
          voteTypedValidator
          (InlineDatum voteDatum)
          (adaValue 2 <> dummyVoteValue)

      combinedTxs = baseTx <> payToTallyValidator <> payToVoteValidator

  -- We want to ensure the `tsProposalEndTime` is before the valid range
  -- Hence using the `from` function here and setting `tsProposalEndTime` to zero
  -- in the sample tally datum
  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user $ finalTx
