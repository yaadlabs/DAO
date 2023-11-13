{- |
Module      : Spec.Vote.Context
Description : Vote policy context unit tests
-}
module Spec.Vote.Context (validVoteConfigNftTest) where

import Control.Monad (void)
import Plutus.Model (
  Run,
  adaValue,
  currentTime,
  mintValue,
  newUser,
  spend,
  submitTx,
  userSpend,
  validateIn,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToScript,
  refInputInline,
 )
import Plutus.V1.Ledger.Interval (to)
import Plutus.V1.Ledger.Value (TokenName (TokenName), Value, singleton)
import Spec.SpecUtils (minAda, oneSecond)
import Spec.Tally.Transactions (runInitTally)
import Spec.Tally.Utils (findTally)
import Spec.Values (
  dummyVoteConfigNftSymbol,
  dummyVoteConfigNftTokenName,
 )
import Spec.Vote.SampleData (sampleVoteDatum)
import Spec.Vote.Script (
  VoteMintingPolicy,
  voteCurrencySymbol,
  voteTypedMintingPolicy,
  voteTypedValidator,
 )
import Spec.Vote.Transactions (runInitVoteConfig)
import Spec.Vote.Utils (findVote)
import Triphut.Vote (VoteMinterActionRedeemer (Mint), VoteMinterConfig (VoteMinterConfig))
import Prelude (mconcat, (*), (+), (<>))

validVoteConfigNftTest :: Run ()
validVoteConfigNftTest = do
  void runInitVoteConfig
  void runInitTally
  (voteOutRef, _, _voteDatum) <- findVote
  (tallyOutRef, _, _tallyDatum) <- findTally

  user <- newUser minAda
  spend1 <- spend user (adaValue 2)
  theTimeNow <- currentTime

  let config = VoteMinterConfig dummyVoteConfigNftSymbol dummyVoteConfigNftTokenName

      voteValue :: Value
      voteValue = singleton (voteCurrencySymbol config) (TokenName "vote") 1

      votePolicy :: VoteMintingPolicy
      votePolicy = voteTypedMintingPolicy config

      -- Set up the txs
      baseTx =
        mconcat
          [ mintValue votePolicy Mint voteValue
          , refInputInline voteOutRef
          , refInputInline tallyOutRef
          , userSpend spend1
          ]

      -- Pay the vote datum, and token,
      -- to the vote validator
      payToVoteValidator =
        payToScript
          voteTypedValidator
          (InlineDatum sampleVoteDatum)
          (adaValue 2 <> voteValue)

  finalTx <- validateIn (to (theTimeNow + 20 * oneSecond)) (baseTx <> payToVoteValidator)

  submitTx user finalTx
