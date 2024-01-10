module Spec.Vote.Transactions (
  runInitVote,
  runInitVoteWithUser,
) where

import Plutus.Model (Run)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import Spec.SpecUtils (runInitPayToScript)
import Spec.Values (dummyVoteValue)
import Spec.Vote.SampleData (sampleVoteDatum, sampleVoteDatumWithUser)
import Spec.Vote.Script (voteTypedValidator)

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
