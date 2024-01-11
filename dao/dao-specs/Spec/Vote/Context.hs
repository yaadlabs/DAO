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
import Dao.ScriptArgument (ConfigurationValidatorConfig)
import LambdaBuffers.ApplicationTypes.Vote (
  VoteMinterActionRedeemer (VoteMinterActionRedeemer'Mint),
 )
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
import PlutusLedgerApi.V1.Interval (to)
import PlutusLedgerApi.V1.Value (TokenName (TokenName), Value, singleton)
import Spec.Configuration.SampleData (sampleConfigValidatorConfig)
import Spec.Configuration.Transactions (runInitConfig)
import Spec.Configuration.Utils (findConfig)
import Spec.SpecUtils (minAda, oneSecond)
import Spec.Tally.Transactions (runInitTallyWithEndTimeInFuture)
import Spec.Tally.Utils (findTally)
import Spec.Values (dummyVoteNFTValue)
import Spec.Vote.SampleData (sampleVoteDatum)
import Spec.Vote.Script (
  VoteMintingPolicy,
  voteCurrencySymbol,
  voteTypedMintingPolicy,
  voteTypedValidator,
 )
import Spec.Vote.Transactions (runInitVoteNft)
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
  (ConfigurationValidatorConfig -> Value) ->
  VoteConfigRef ->
  ValidityRange ->
  Run ()
mkVoteConfigNftTest voteConfigValue voteConfigRef validityRange = do
  runInitConfig
  -- Simulate a voteNFT at the user's wallet
  runInitVoteNft
  void runInitTallyWithEndTimeInFuture

  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, _tallyDatum) <- findTally

  user <- newUser (minAda <> dummyVoteNFTValue)
  spend' <- spend user (adaValue 2 <> dummyVoteNFTValue)
  theTimeNow <- currentTime

  let
    voteValue :: Value
    voteValue = voteConfigValue sampleConfigValidatorConfig

    votePolicy :: VoteMintingPolicy
    votePolicy = voteTypedMintingPolicy sampleConfigValidatorConfig

    -- Set up the txs
    baseTx =
      mconcat
        [ mintValue votePolicy VoteMinterActionRedeemer'Mint voteValue
        , refInputInline tallyOutRef
        , userSpend spend'
        ]

    withVoteConfig = case voteConfigRef of
      ConfigInRefInputs -> refInputInline configOutRef
      NoConfigInRefInputs -> mempty

    -- Pay the vote datum, vote token and vote NFT,
    -- to the vote validator
    payToVoteValidator =
      payToScript
        voteTypedValidator
        (InlineDatum sampleVoteDatum)
        (adaValue 2 <> voteValue <> dummyVoteNFTValue)

    combinedTxs = mconcat [baseTx, payToVoteValidator, withVoteConfig]

  finalTx <- validateIn (to (theTimeNow + 20 * oneSecond)) combinedTxs

  case validityRange of
    SpecifyRange -> submitTx user finalTx
    NoSpecificRange -> submitTx user combinedTxs -- Should (will) fail

-- Valid token value, correct symbol and exactly one minted
validVoteConfigValue :: ConfigurationValidatorConfig -> Value
validVoteConfigValue config = singleton (voteCurrencySymbol config) (TokenName "vote") 1

validVoteConfigValue' :: ConfigurationValidatorConfig -> Value
validVoteConfigValue' config =
  mconcat
    [ singleton (voteCurrencySymbol config) (TokenName "vote") 1
    , dummyVoteNFTValue
    ]

-- Valid token value, correct symbol and exactly one minted
invalidMoreThanOneVoteConfigValue :: ConfigurationValidatorConfig -> Value
invalidMoreThanOneVoteConfigValue config = singleton (voteCurrencySymbol config) (TokenName "vote") 2
