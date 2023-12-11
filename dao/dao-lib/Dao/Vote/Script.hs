{- |
Module: Dao.Vote.Script
Description: Dao vote related scripts. It includes:
  - Vote minting policy script.
  - Vote validator script.
-}
module Dao.Vote.Script (
  -- * Minting policy
  mkVoteMinter,
  wrappedPolicy,

  -- * Validator
  voteValidatorCompiledCode,
) where

import Dao.ScriptArgument (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
 )
import Dao.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasBurnedTokens,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  hasTokenInValue,
  wrapValidate'',
 )
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Data.Maybe (Maybe (Just))
import LambdaBuffers.ApplicationTypes.Configuration (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dynamicConfigDatum'fungibleVotePercent,
    dynamicConfigDatum'tallyNft,
    dynamicConfigDatum'tallyValidator,
    dynamicConfigDatum'voteCurrencySymbol,
    dynamicConfigDatum'voteFungibleCurrencySymbol,
    dynamicConfigDatum'voteFungibleTokenName,
    dynamicConfigDatum'voteNft,
    dynamicConfigDatum'voteTokenName,
    dynamicConfigDatum'voteValidator
  ),
 )
import LambdaBuffers.ApplicationTypes.Tally (
  TallyStateDatum (
    TallyStateDatum,
    tallyStateDatum'proposalEndTime
  ),
 )
import LambdaBuffers.ApplicationTypes.Vote (
  VoteActionRedeemer (VoteActionRedeemer'Cancel, VoteActionRedeemer'Count),
  VoteDatum (VoteDatum, voteDatum'returnAda, voteDatum'voteOwner),
  VoteMinterActionRedeemer (VoteMinterActionRedeemer'Burn, VoteMinterActionRedeemer'Mint),
 )
import PlutusLedgerApi.V1.Address (addressCredential)
import PlutusLedgerApi.V1.Credential (Credential (PubKeyCredential, ScriptCredential))
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Interval (after)
import PlutusLedgerApi.V1.Value (
  Value,
  adaSymbol,
  adaToken,
  valueOf,
 )
import PlutusLedgerApi.V2 (
  Datum,
  DatumHash,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInInfo (TxInInfo, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoValidRange
  ),
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (
    TxOut,
    txOutAddress,
    txOutDatum,
    txOutValue
  ),
 )
import PlutusTx (CompiledCode, applyCode, compile, fromBuiltinData, liftCode)
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (
  Bool,
  BuiltinData,
  any,
  check,
  filter,
  not,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
  (==),
  (>),
 )

{- | Policy for minting or burning the vote NFT.

   == Minting Vote Token

      When the 'Dao.Vote.VoteMinterActionRedeemer' redeemer
      is set to 'Mint', this policy performs the following checks:

        - There is exactly one 'Dao.Types.DynamicConfigDatum' in the reference inputs,
          marked by the config NFT
          (Corresponding config 'CurrencySymbol' and 'TokenName' provided by the 'ConfigurationValidatorConfig' argument)
        - There is exactly one 'Dao.Types.TallyStateDatum' in the reference inputs,
          marked by the Tally NFT
        - Exactly one valid Vote NFT is minted with the valid token name.
        - The token name matches the 'dcVoteTokenName' field of the 'DynamicConfigDatum'.
        - There is exactly one output containing the vote NFT.
        - This output contains a valid 'Dao.Vote.VoteDatum' datum.
        - The proposal is still active.
          Checked by ensuring the proposal end time provided by
          the 'TallyStateDatum' is after the validity range of the transaction
        - The total ada is greater than the return ada specified by the 'voteDatum'returnAda' field of the 'VoteDatum'

   == Burning Vote Token

      When the 'Dao.Vote.VoteMinterActionRedeemer' redeemer
      is set to 'Burn', this policy performs the following checks:

        - That one vote token is burned
-}
mkVoteMinter :: ConfigurationValidatorConfig -> VoteMinterActionRedeemer -> ScriptContext -> Bool
mkVoteMinter
  ConfigurationValidatorConfig {..}
  action
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } = case action of
    VoteMinterActionRedeemer'Burn ->
      let
        -- Check the transaction burns a valid token
        burnsTokens :: Bool
        !burnsTokens = hasBurnedTokens thisCurrencySymbol txInfoMint "Vote Minter Burn"
       in
        traceIfFalse "Need to burn a vote token" burnsTokens
    VoteMinterActionRedeemer'Mint ->
      let
        -- Helper for filtering for config UTXO in the reference inputs
        hasConfigurationNft :: Value -> Bool
        hasConfigurationNft = hasOneOfToken cvcConfigNftCurrencySymbol cvcConfigNftTokenName

        -- The datums
        theData :: Map DatumHash Datum
        theData = txInfoData

        -- Get the configuration from the reference inputs
        DynamicConfigDatum {..} =
          case filter
            (hasConfigurationNft . txOutValue . txInInfoResolved)
            txInfoReferenceInputs of
            [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum theData txOutDatum
            _ -> traceError "Should be exactly one valid config in the reference inputs"

        -- Get output at the vote validator, should just be one.
        (VoteDatum {..}, !voteValue) =
          case filter
            ((== ScriptCredential dynamicConfigDatum'voteValidator) . addressCredential . txOutAddress)
            txInfoOutputs of
            [TxOut {..}] -> (convertDatum theData txOutDatum, txOutValue)
            _ -> traceError "Should be exactly one vote datum (proposal reference) at the output"

        -- Helper for filtering for tally UTXO in the outputs
        hasTallyNft :: Value -> Bool
        hasTallyNft = hasSymbolInValue dynamicConfigDatum'tallyNft

        -- Get the tally state datum at the output marked by the tally NFT
        TallyStateDatum {tallyStateDatum'proposalEndTime} =
          case filter
            (hasTallyNft . txOutValue . txInInfoResolved)
            txInfoReferenceInputs of
            [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum theData txOutDatum
            _ -> traceError "Should be exactly one tally state datum in the reference inputs"

        -- Ensure the proposal end time is after the transaction's validity range
        proposalIsActive :: Bool
        !proposalIsActive = tallyStateDatum'proposalEndTime `after` txInfoValidRange

        -- Ensure the vote value contains exactly one valid witness token
        hasWitness :: Bool
        !hasWitness = hasOneOfToken thisCurrencySymbol dynamicConfigDatum'voteTokenName voteValue

        -- Ensure exactly one valid vote token is minted
        onlyMintedOne :: Bool
        !onlyMintedOne =
          hasSingleTokenWithSymbolAndTokenName
            txInfoMint
            thisCurrencySymbol
            dynamicConfigDatum'voteTokenName

        hasVoteNft :: Bool
        !hasVoteNft = hasTokenInValue dynamicConfigDatum'voteNft "Vote NFT" voteValue

        -- Ensure the return ADA is less than the ada provided by the user, contained in the vote value
        totalAdaIsGreaterThanReturnAda :: Bool
        !totalAdaIsGreaterThanReturnAda = valueOf voteValue adaSymbol adaToken > voteDatum'returnAda
       in
        traceIfFalse "Proposal has expired" proposalIsActive
          && traceIfFalse "Vote Nft is missing" hasVoteNft
          && traceIfFalse "Missing witness on output" hasWitness
          && traceIfFalse "Should be exactly one valid token minted" onlyMintedOne
          && traceIfFalse "Total ada is not high enough" totalAdaIsGreaterThanReturnAda
mkVoteMinter _ _ _ = traceError "Wrong type of script purpose!"

wrappedPolicy :: ConfigurationValidatorConfig -> WrappedMintingPolicyType
wrappedPolicy config x y =
  let (maybeDataX, maybeDataY) = (fromBuiltinData x, fromBuiltinData y)
   in case (maybeDataX, maybeDataY) of
        (Just dataX, Just dataY) -> check (mkVoteMinter config dataX dataY)
        _ -> traceError "Error at fromBuiltinData function"

{- | Validator for votes.

   == Common checks

     The validator always ensures:

       - There is exactly one 'Dao.Types.DynamicConfigDatum' in the reference inputs,
         marked by the config NFT. (Corresponding config 'CurrencySymbol' and 'TokenName'
         provided by the 'ConfigurationValidatorConfig' argument)

   == Count vote

      When the 'Dao.Vote.VoteActionRedeemer' redeemer
      is set to 'Count', this validator performs the following checks:

        - That the tally validator is present in the inputs, the tally validator is specified
          by the 'dcTallyValidator' field of the 'DynamicConfigDatum'

   == Cancel vote

      When the 'Dao.Vote.VoteActionRedeemer' redeemer
      is set to 'Cancel', this validator performs the following checks:

        - The transaction is signed by the vote owner, specified by the 'voteDatum'voteOwner' field
          of the 'Dao.Vote.VoteDatum'.
        - All the vote tokens are burned, checking that there are no vote tokens in the transaction outputs,
          with the corresponding 'CurrencySymbol' specified by the 'dcVoteCurrencySymbol'
          in the 'DynamicConfigDatum'
-}
validateVote ::
  ConfigurationValidatorConfig ->
  VoteDatum ->
  VoteActionRedeemer ->
  ScriptContext ->
  Bool
validateVote
  ConfigurationValidatorConfig {..}
  VoteDatum {..}
  action
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    } =
    let
      -- Helper for filtering for config UTXO in the reference inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken cvcConfigNftCurrencySymbol cvcConfigNftTokenName

      -- Get the configuration from the reference inputs
      DynamicConfigDatum {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one config NFT in the reference inputs. None found."
     in
      case action of
        VoteActionRedeemer'Count ->
          -- Ensure the tally validator is contained in the inputs
          traceIfFalse
            "Missing Tally Validator input"
            ( any
                ( (== ScriptCredential dynamicConfigDatum'tallyValidator)
                    . addressCredential
                    . txOutAddress
                    . txInInfoResolved
                )
                (txInfoInputs :: [TxInInfo])
            )
        VoteActionRedeemer'Cancel ->
          let
            -- Ensure that the tx is signed by the vote owner,
            -- specified in the 'voteDatum'voteOwner' field of the 'VoteDatum'
            isSignedByOwner :: Bool
            !isSignedByOwner =
              any
                ((== addressCredential voteDatum'voteOwner) . PubKeyCredential)
                (txInfoSignatories :: [PubKeyHash])

            -- Helper for filtering for UTXOs containing a vote token
            hasVoteToken :: Value -> Bool
            hasVoteToken = hasSymbolInValue dynamicConfigDatum'voteCurrencySymbol

            -- Ensure there are no vote tokens in the outputs
            voteTokenAreAllBurned :: Bool
            !voteTokenAreAllBurned =
              not $ any (hasVoteToken . txOutValue) (txInfoOutputs :: [TxOut])
           in
            traceIfFalse "Transaction should be signed by the vote owner" isSignedByOwner
              && traceIfFalse "All vote tokens should be burned" voteTokenAreAllBurned

voteValidatorCompiledCode ::
  ConfigurationValidatorConfig ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
voteValidatorCompiledCode config =
  $$(PlutusTx.compile [||wrapValidate'' validateVote||]) `applyCode` liftCode config
