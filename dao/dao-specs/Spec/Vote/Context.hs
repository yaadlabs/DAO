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
import Dao.ScriptArgument (ValidatorParams)
import Debug.Trace (traceM)
import LambdaBuffers.ApplicationTypes.Vote (
  VoteMinterActionRedeemer (VoteMinterActionRedeemer'Mint),
 )
import Plutus.Model (
  Run,
  TypedPolicy,
  adaValue,
  currentTime,
  getMainUser,
  logInfo,
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
  spendScript,
 )
import PlutusLedgerApi.V1.Interval (to)
import PlutusLedgerApi.V1.Value (TokenName (TokenName), Value, singleton)
import Spec.Configuration.SampleData (sampleValidatorParams)
import Spec.Configuration.Transactions (runInitConfig)
import Spec.Configuration.Utils (findConfig)
import Spec.Index.Transactions (runInitIndex)
import Spec.SpecUtils (amountOfAda, findConfigUtxo, minAda, oneSecond, runInitPayToScript)
import Spec.Tally.SampleData (sampleUpgradeWithEndTimeInFutureTallyStateDatum)
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Tally.Utils (findTally)
import Spec.Values (
  dummyIndexConfigNftValue,
  dummyTallySymbol,
  dummyTallyTokenName,
  dummyTallyValue,
  dummyVoteNFTValue,
 )
import Spec.Vote.SampleData (sampleVoteDatum)
import Spec.Vote.Script (
  VoteMintingPolicy,
  voteCurrencySymbol,
  voteTypedMintingPolicy,
  voteTypedValidator,
 )
import Spec.Vote.Transactions (runInitVoteNft)
import Prelude (mconcat, mempty, show, (*), (+), (<>))

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
  (ValidatorParams -> Value) ->
  VoteConfigRef ->
  ValidityRange ->
  Run ()
mkVoteConfigNftTest voteConfigValue voteConfigRef validityRange = do
  let logMsg msg = do
        logInfo msg
        traceM msg

  logMsg "TX1: runInitConfig"
  void runInitConfig
  logMsg "TX2: runInitIndex"
  void runInitIndex
  logMsg "TX3: runInitVoteNft"
  runInitVoteNft
  logMsg "TX4: Initialize dummy Tally (simple)"
  -- Use dummy tally value instead of minting real one
  runInitPayToScript
    tallyNftTypedValidator
    sampleUpgradeWithEndTimeInFutureTallyStateDatum
    dummyTallyValue
  logMsg "All init transactions complete"

  logMsg "Finding config..."
  (configOutRef, _, _) <- findConfig
  logMsg ("✓ Found config: " <> show configOutRef)

  logMsg "Finding tally (using dummy tally value)..."
  -- findTally uses the computed tally symbol, but we used dummyTallyValue
  -- So we need to find it using the dummy symbol instead
  (tallyOutRef, _, _tallyDatum) <- findConfigUtxo tallyNftTypedValidator dummyTallySymbol dummyTallyTokenName
  logMsg ("✓ Found tally: " <> show tallyOutRef)

  -- Admin has vote NFT from TX3 (runInitVoteNft)
  admin <- getMainUser
  logMsg "Admin spending: minAda + 2 ADA + dummyVoteNFTValue for vote minting"
  spend' <- spend admin (minAda <> adaValue 2 <> dummyVoteNFTValue)
  theTimeNow <- currentTime

  let
    voteValue :: Value
    voteValue = voteConfigValue sampleValidatorParams

    votePolicy :: VoteMintingPolicy
    votePolicy = voteTypedMintingPolicy sampleValidatorParams

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
        (minAda <> adaValue 2 <> voteValue <> dummyVoteNFTValue)

    combinedTxs = mconcat [baseTx, payToVoteValidator, withVoteConfig]

  finalTx <- validateIn (to (theTimeNow + 20 * oneSecond)) combinedTxs

  logMsg "Submitting vote minting transaction..."
  case validityRange of
    SpecifyRange -> submitTx admin finalTx
    NoSpecificRange -> submitTx admin combinedTxs -- Should (will) fail
  logMsg "✓ Vote minted successfully!"

-- Valid token value, correct symbol and exactly one minted
validVoteConfigValue :: ValidatorParams -> Value
validVoteConfigValue config = singleton (voteCurrencySymbol config) (TokenName "vote") 1

validVoteConfigValue' :: ValidatorParams -> Value
validVoteConfigValue' config =
  mconcat
    [ singleton (voteCurrencySymbol config) (TokenName "vote") 1
    , dummyVoteNFTValue
    ]

-- Valid token value, correct symbol and exactly one minted
invalidMoreThanOneVoteConfigValue :: ValidatorParams -> Value
invalidMoreThanOneVoteConfigValue config = singleton (voteCurrencySymbol config) (TokenName "vote") 2
