module Spec.Vote.Transactions (
  runInitVote,
  runInitVoteWithUser,
  runInitVoteNft,
) where

import Plutus.Model (Run)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import Spec.SpecUtils (payToPkhTx, runInitPayToScript)
import Spec.Values (dummyVoteNFTValue, dummyVoteValue)
import Spec.Vote.SampleData (sampleVoteDatum, sampleVoteDatumWithUser)
import Spec.Vote.Script (voteTypedValidator)

runInitVote :: Run ()
runInitVote =
  runInitPayToScript
    voteTypedValidator
    sampleVoteDatum
    dummyVoteValue

runInitVoteNft :: Run ()
runInitVoteNft = payToPkhTx dummyVoteNFTValue

runInitVoteWithUser :: PubKeyHash -> Run ()
runInitVoteWithUser pkh =
  runInitPayToScript
    voteTypedValidator
    (sampleVoteDatumWithUser pkh)
    dummyVoteValue
