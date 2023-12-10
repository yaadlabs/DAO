{-
Module      : Spec.Vote.SampleData
Description : Vote sample data for tests
-}
module Spec.Vote.SampleData (
  sampleVoteDatumWithUser,
  sampleVoteDatum,
) where

import LambdaBuffers.ApplicationTypes.Vote (
  VoteDatum (
    VoteDatum,
    voteDatum'direction,
    voteDatum'proposalTokenName,
    voteDatum'returnAda,
    voteDatum'voteOwner
  ),
  VoteDirection (VoteDirection'For),
 )
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (adaToken)
import Spec.Addresses (dummyVoterAddress)

sampleVoteDatum :: VoteDatum
sampleVoteDatum =
  VoteDatum
    { voteDatum'proposalTokenName = adaToken
    , voteDatum'direction = VoteDirection'For
    , voteDatum'voteOwner = dummyVoterAddress
    , voteDatum'returnAda = 1
    }

sampleVoteDatumWithUser :: PubKeyHash -> VoteDatum
sampleVoteDatumWithUser user =
  VoteDatum
    { voteDatum'proposalTokenName = adaToken
    , voteDatum'direction = VoteDirection'For
    , voteDatum'voteOwner = pubKeyHashAddress user
    , voteDatum'returnAda = 1
    }
