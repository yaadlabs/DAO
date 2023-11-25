{- |
Module      : Spec.Vote.SampleData
Description : Vote sample data for tests
-}
module Spec.Vote.SampleData (
  sampleVoteValidatorConfig,
  sampleVoteMinterConfig,
  sampleVoteDatumWithUser,
  sampleVoteDatum,
) where

import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (adaToken)
import Spec.Addresses (dummyVoterAddress)
import Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyVoteConfigNftSymbol,
  dummyVoteConfigNftTokenName,
 )
import Triphut.Vote (
  VoteDatum (..),
  VoteDirection (For),
  VoteMinterConfig (VoteMinterConfig),
  VoteValidatorConfig (VoteValidatorConfig),
 )

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

sampleVoteValidatorConfig :: VoteValidatorConfig
sampleVoteValidatorConfig = VoteValidatorConfig dummyConfigNftSymbol dummyConfigNftTokenName

sampleVoteMinterConfig :: VoteMinterConfig
sampleVoteMinterConfig = VoteMinterConfig dummyVoteConfigNftSymbol dummyVoteConfigNftTokenName
