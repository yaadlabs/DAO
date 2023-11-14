module Spec.Vote.ContextValidator (validVoteValidatorTest) where

import Plutus.Model (
  Run,
  adaValue,
  newUser,
  spend,
  spendScript,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToScript,
  refInputInline,
 )
import PlutusTx.Prelude (($))
import Spec.SpecUtils (minAda)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Tally.Transactions (runInitTally)
import Spec.Tally.Utils (findTally)
import Spec.Values (dummyTallyValue, dummyVoteValue)
import Spec.Vote.Script (voteTypedValidator)
import Spec.Vote.Transactions (runInitVote, runInitVoteConfig)
import Spec.Vote.Utils (findVote, findVoteConfig)
import Triphut.Vote (VoteActionRedeemer (Count))
import Prelude (mconcat, pure, (<>))

validVoteValidatorTest :: Run ()
validVoteValidatorTest = mkVoteValidatorTest

mkVoteValidatorTest :: Run ()
mkVoteValidatorTest = do
  runInitVoteConfig
  runInitTally
  runInitVote

  (configOutRef, _, _) <- findVoteConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (voteOutRef, _, voteDatum) <- findVote

  user <- newUser minAda
  spend' <- spend user (adaValue 2)
  spend2 <- spend user (adaValue 2)

  let baseTx =
        mconcat
          [ spendScript tallyNftTypedValidator tallyOutRef () tallyDatum
          , spendScript voteTypedValidator voteOutRef Count voteDatum
          , userSpend spend'
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

  submitTx user $ baseTx <> payToTallyValidator <> payToVoteValidator
