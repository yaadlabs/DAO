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
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (adaToken)
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
