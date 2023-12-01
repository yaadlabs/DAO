{- |
Module      : Spec.Vote.Context
Description : Vote policy unit tests
-}
module Spec.Vote.Context (
  validVoteConfigNftTest,
  invalidMoreThanOneTokenVoteConfigNftTest,
  invalidNoConfigInRefInputsVoteConfigNftTest,
  invalidProposalEndTimeNotAfterValidityRangeVoteConfigNftTest,
) where

import Control.Monad (void)
import Dao.Vote (VoteMinterActionRedeemer (Mint), VoteMinterConfig (VoteMinterConfig))
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
import Spec.Tally.Transactions (runInitTallyWithEndTimeInFuture)
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
import Spec.Vote.Transactions (runInitVoteMinterConfig)
import Spec.Vote.Utils (findVoteMinterConfig)
import Prelude (mconcat, mempty, (*), (+), (<>))

validVoteConfigNftTest :: Run ()
validVoteConfigNftTest =
  mkVoteConfigNftTest
    validVoteConfigValue
    ConfigInRefInputs
    SpecifyRange

invalidMoreThanOneTokenVoteConfigNftTest :: Run ()
invalidMoreThanOneTokenVoteConfigNftTest =
  mkVoteConfigNftTest
    invalidMoreThanOneVoteConfigValue
    ConfigInRefInputs
    SpecifyRange

invalidNoConfigInRefInputsVoteConfigNftTest :: Run ()
invalidNoConfigInRefInputsVoteConfigNftTest =
  mkVoteConfigNftTest
    validVoteConfigValue
    NoConfigInRefInputs
    SpecifyRange

invalidProposalEndTimeNotAfterValidityRangeVoteConfigNftTest :: Run ()
invalidProposalEndTimeNotAfterValidityRangeVoteConfigNftTest =
  mkVoteConfigNftTest
    validVoteConfigValue
    ConfigInRefInputs
    NoSpecificRange

data VoteConfigRef
  = ConfigInRefInputs
  | NoConfigInRefInputs

data ValidityRange
  = SpecifyRange
  | NoSpecificRange

mkVoteConfigNftTest ::
  (VoteMinterConfig -> Value) ->
  VoteConfigRef ->
  ValidityRange ->
  Run ()
mkVoteConfigNftTest voteConfigValue voteConfigRef validityRange = do
  void runInitVoteMinterConfig
  void runInitTallyWithEndTimeInFuture

  (voteConfigOutRef, _, _voteDatum) <- findVoteMinterConfig
  (tallyOutRef, _, _tallyDatum) <- findTally

  user <- newUser minAda
  spend1 <- spend user (adaValue 2)
  theTimeNow <- currentTime

  let config = VoteMinterConfig dummyVoteConfigNftSymbol dummyVoteConfigNftTokenName

      voteValue :: Value
      voteValue = voteConfigValue config

      votePolicy :: VoteMintingPolicy
      votePolicy = voteTypedMintingPolicy config

      -- Set up the txs
      baseTx =
        mconcat
          [ mintValue votePolicy Mint voteValue
          , refInputInline tallyOutRef
          , userSpend spend1
          ]

      withVoteConfig = case voteConfigRef of
        ConfigInRefInputs -> refInputInline voteConfigOutRef
        NoConfigInRefInputs -> mempty

      -- Pay the vote datum, and token,
      -- to the vote validator
      payToVoteValidator =
        payToScript
          voteTypedValidator
          (InlineDatum sampleVoteDatum)
          (adaValue 2 <> voteValue)

      combinedTxs = mconcat [baseTx, payToVoteValidator, withVoteConfig]

  finalTx <- validateIn (to (theTimeNow + 20 * oneSecond)) combinedTxs

  case validityRange of
    SpecifyRange -> submitTx user finalTx
    NoSpecificRange -> submitTx user combinedTxs -- Should (will) fail

-- Valid token value, correct symbol and exactly one minted
validVoteConfigValue :: VoteMinterConfig -> Value
validVoteConfigValue config = singleton (voteCurrencySymbol config) (TokenName "vote") 1

-- Valid token value, correct symbol and exactly one minted
invalidMoreThanOneVoteConfigValue :: VoteMinterConfig -> Value
invalidMoreThanOneVoteConfigValue config = singleton (voteCurrencySymbol config) (TokenName "vote") 2
