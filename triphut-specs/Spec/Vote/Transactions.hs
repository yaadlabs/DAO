module Spec.Vote.Transactions (
  runInitVoteMinterConfig,
  runInitVote,
  runInitVoteWithUser,
) where

import Plutus.Model (Run)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1)
import Spec.SampleData (sampleVoteMinterDynamicConfig)
import Spec.SpecUtils (minAda, runInitPayToScript, runInitReferenceScript)
import Spec.Values (dummyVoteConfigNftValue, dummyVoteValue)
import Spec.Vote.SampleData (sampleVoteDatum, sampleVoteDatumWithUser)
import Spec.Vote.Script (voteTypedValidator)
import Prelude ((<>))

runInitVoteMinterConfig :: Run ()
runInitVoteMinterConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator1
    sampleVoteMinterDynamicConfig
    (dummyVoteConfigNftValue <> minAda)

runInitVote :: Run ()
runInitVote =
  runInitPayToScript
    voteTypedValidator
    sampleVoteDatum
    dummyVoteValue

runInitVoteWithUser :: PubKeyHash -> Run ()
runInitVoteWithUser pkh =
  runInitPayToScript
    voteTypedValidator
    (sampleVoteDatumWithUser pkh)
    dummyVoteValue
