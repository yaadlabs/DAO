{- |
Module      : Spec.Vote.SampleData
Description : Vote sample data for tests
-}
module Spec.Vote.SampleData (
  sampleVoteValidatorConfig,
  sampleVoteMinterConfig,
  sampleVoteDatum,
) where

import Plutus.V1.Ledger.Value (adaSymbol, adaToken)
import Spec.Addresses (sampleVoterAddress)
import Spec.Values (dummyVoteConfigNftSymbol, dummyVoteConfigNftTokenName)
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
    , vOwner = sampleVoterAddress
    , vReturnAda = 1
    }

sampleVoteValidatorConfig :: VoteValidatorConfig
sampleVoteValidatorConfig = VoteValidatorConfig dummyVoteConfigNftSymbol dummyVoteConfigNftTokenName

sampleVoteMinterConfig :: VoteMinterConfig
sampleVoteMinterConfig = VoteMinterConfig dummyVoteConfigNftSymbol dummyVoteConfigNftTokenName
