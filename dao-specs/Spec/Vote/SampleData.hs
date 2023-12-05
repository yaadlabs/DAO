{- |
Module      : Spec.Vote.SampleData
Description : Vote sample data for tests
-}
module Spec.Vote.SampleData (
  sampleVoteDatumWithUser,
  sampleVoteDatum,
) where

import Dao.Vote (
  VoteDatum (..),
  VoteDirection (For),
 )
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (adaToken)
import Spec.Addresses (dummyVoterAddress)

sampleVoteDatum :: VoteDatum
sampleVoteDatum =
  VoteDatum
    { vProposalTokenName = adaToken
    , vDirection = For
    , vOwner = dummyVoterAddress
    , vReturnAda = 1
    }

sampleVoteDatumWithUser :: PubKeyHash -> VoteDatum
sampleVoteDatumWithUser user =
  VoteDatum
    { vProposalTokenName = adaToken
    , vDirection = For
    , vOwner = pubKeyHashAddress user
    , vReturnAda = 1
    }
