module Spec.Vote.Transactions (
  runInitVoteConfig,
  runInitVote,
) where

import Plutus.Model (
  Run,
  getMainUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToRef,
  payToScript,
 )
import PlutusTx.Prelude (($))
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1)
import Spec.SampleData (sampleVoteDynamicConfig)
import Spec.SpecUtils (initScriptRef, minAda, runInitPayToScript, runInitReferenceScript)
import Spec.Values (dummyVoteConfigNftValue, dummyVoteValue)
import Spec.Vote.SampleData (sampleVoteDatum)
import Spec.Vote.Script (voteTypedValidator)
import Prelude ((<>))

runInitVoteConfig :: Run ()
runInitVoteConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator1
    sampleVoteDynamicConfig
    (dummyVoteConfigNftValue <> minAda)

runInitVote :: Run ()
runInitVote =
  runInitPayToScript
    voteTypedValidator
    sampleVoteDatum
    dummyVoteValue
