{- |
Module: Triphut.Vote.Script
Description: Triphut vote related scripts. It includes:
  - Vote minting policy script.
  - Vote validator script.
-}
module Triphut.Vote.Script (
  -- * Minting policy
  voteMinter,
  voteMinterPolicyId,
  mkVoteMinter,
  wrappedPolicy,

  -- * Validator
  voteScript,
  voteValidator,
  voteValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Address (addressCredential)
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval (after)
import Plutus.V1.Ledger.Scripts (
  Datum,
  DatumHash,
  MintingPolicy,
  Validator (Validator),
  ValidatorHash,
  mkMintingPolicyScript,
  unMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  Value,
  adaSymbol,
  adaToken,
  mpsSymbol,
  valueOf,
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (
    ScriptContext,
    scriptContextPurpose,
    scriptContextTxInfo
  ),
  ScriptPurpose (Minting),
  TxInInfo (
    TxInInfo,
    txInInfoResolved
  ),
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
  TxOut (
    TxOut,
    txOutAddress,
    txOutDatum,
    txOutValue
  ),
 )
import Plutus.V2.Ledger.Tx hiding (Mint)
import PlutusTx (applyCode, compile, liftCode, unsafeFromBuiltinData)
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (
  Bool (False),
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
import Triphut.ConfigurationNft (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
 )
import Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasBurnedTokens,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  hasTokenInValue,
  mkValidatorWithSettings,
  plutonomyMintingPolicyHash,
  validatorHash,
  validatorToScript,
  wrapValidate,
 )
import Triphut.Types (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dcTallyNft,
    dcTallyValidator,
    dcVoteCurrencySymbol,
    dcVoteNft,
    dcVoteTokenName,
    dcVoteValidator
  ),
  TallyStateDatum (TallyStateDatum, tsProposalEndTime),
 )
import Triphut.Vote (
  VoteActionRedeemer (Cancel, Count),
  VoteDatum (VoteDatum, vOwner, vReturnAda),
  VoteMinterActionRedeemer (Burn, Mint),
 )

{- | Policy for minting or burning the vote NFT.

   == Minting Vote Token

      When the 'Triphut.Vote.VoteMinterActionRedeemer' redeemer
      is set to 'Mint', this policy performs the following checks:

        - There is exactly one 'Triphut.Types.DynamicConfigDatum' in the reference inputs,
          marked by the config NFT
          (Corresponding config 'CurrencySymbol' and 'TokenName' provided by the 'ConfigurationValidatorConfig' argument)
        - There is exactly one 'Triphut.Types.TallyStateDatum' in the reference inputs,
          marked by the Tally NFT
        - Exactly one valid Vote NFT is minted with the valid token name.
        - The token name matches the 'dcVoteTokenName' field of the 'DynamicConfigDatum'.
        - There is exactly one output containing the vote NFT.
        - This output contains a valid 'Triphut.Vote.VoteDatum' datum.
        - The proposal is still active.
          Checked by ensuring the proposal end time provided by
          the 'TallyStateDatum' is after the validity range of the transaction
        - The total ada is greater than the return ada specificed by the 'vReturnAda' field of the 'VoteDatum'

   == Burning Vote Token

      When the 'Triphut.Vote.VoteMinterActionRedeemer' redeemer
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
    Burn ->
      let
        -- Check the transaction burns a valid token
        burnsTokens :: Bool
        !burnsTokens = hasBurnedTokens thisCurrencySymbol txInfoMint "Vote Minter Burn"
       in
        traceIfFalse "Need to burn a vote token" burnsTokens
    Mint ->
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
            ((== ScriptCredential dcVoteValidator) . addressCredential . txOutAddress)
            txInfoOutputs of
            [TxOut {..}] -> (convertDatum theData txOutDatum, txOutValue)
            _ -> traceError "Should be exactly one vote datum (proposal reference) at the output"

        -- Helper for filtering for tally UTXO in the outputs
        hasTallyNft :: Value -> Bool
        hasTallyNft = hasSymbolInValue dcTallyNft

        -- Get the tally state datum at the output marked by the tally NFT
        TallyStateDatum {tsProposalEndTime} =
          case filter
            (hasTallyNft . txOutValue . txInInfoResolved)
            txInfoReferenceInputs of
            [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum theData txOutDatum
            _ -> traceError "Should be exactly one tally state datum in the reference inputs"

        -- Ensure the proposal end time is after the transaction's validity range
        proposalIsActive :: Bool
        !proposalIsActive = tsProposalEndTime `after` txInfoValidRange

        -- Ensure the vote value contains exactly one valid witness token
        hasWitness :: Bool
        !hasWitness = hasOneOfToken thisCurrencySymbol dcVoteTokenName voteValue

        -- Ensure exactly one valid vote token is minted
        onlyMintedOne :: Bool
        !onlyMintedOne =
          hasSingleTokenWithSymbolAndTokenName
            txInfoMint
            thisCurrencySymbol
            dcVoteTokenName

        hasVoteNft :: Bool
        !hasVoteNft = hasTokenInValue dcVoteNft "Vote NFT" voteValue

        -- Ensure the return ADA is less than the ada provided by the user, contained in the vote value
        totalAdaIsGreaterThanReturnAda :: Bool
        !totalAdaIsGreaterThanReturnAda = valueOf voteValue adaSymbol adaToken > vReturnAda
       in
        traceIfFalse "Proposal has expired" proposalIsActive
          && traceIfFalse "Vote Nft is missing" hasVoteNft
          && traceIfFalse "Missing witness on output" hasWitness
          && traceIfFalse "Should be exactly one valid token minted" onlyMintedOne
          && traceIfFalse "Total ada is not high enough" totalAdaIsGreaterThanReturnAda
mkVoteMinter _ _ _ = traceError "Wrong type of script purpose!"

wrappedPolicy :: ConfigurationValidatorConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkVoteMinter config (unsafeFromBuiltinData a) (unsafeFromBuiltinData b))

policy :: ConfigurationValidatorConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

voteMinterPolicyId :: ConfigurationValidatorConfig -> CurrencySymbol
voteMinterPolicyId = mpsSymbol . plutonomyMintingPolicyHash . policy

scriptAsCbor :: ConfigurationValidatorConfig -> BSL.ByteString
scriptAsCbor =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        , Plutonomy.ooFloatOutLambda = False
        }
   in
    serialise
      . Plutonomy.optimizeUPLCWith optimizerSettings
      . Validator
      . unMintingPolicyScript
      . policy

voteMinter :: ConfigurationValidatorConfig -> PlutusScript PlutusScriptV2
voteMinter =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . scriptAsCbor

{- | Validator for votes.

   == Common checks

     The validator always ensures:

       - There is exactly one 'Triphut.Types.DynamicConfigDatum' in the reference inputs,
          marked by the config NFT. (Corresponding config 'CurrencySymbol' and 'TokenName'
          provided by the 'ConfigurationValidatorConfig' argument)

   == Count vote

      When the 'Triphut.Vote.VoteActionRedeemer' redeemer
      is set to 'Count', this validator performs the following checks:

        - That the tally validator is present in the inputs, the tally validator is specified
          by the 'dcTallyValidator' field of the 'DynamicConfigDatum'

   == Cancel vote

      When the 'Triphut.Vote.VoteActionRedeemer' redeemer
      is set to 'Cancel', this validator performs the following checks:

        - The transaction is signed by the vote owner, specified by the 'vOwner' field
          of the 'Triphut.Vote.VoteDatum'.
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
        Count ->
          -- Ensure the tally validator is contained in the inputs
          traceIfFalse
            "Missing Tally Validator input"
            ( any
                ( (== ScriptCredential dcTallyValidator)
                    . addressCredential
                    . txOutAddress
                    . txInInfoResolved
                )
                (txInfoInputs :: [TxInInfo])
            )
        Cancel ->
          let
            -- Ensure that the tx is signed by the vote owner,
            -- specified in the 'vOwner' field of the 'VoteDatum'
            isSignedByOwner :: Bool
            !isSignedByOwner =
              any
                ((== addressCredential vOwner) . PubKeyCredential)
                (txInfoSignatories :: [PubKeyHash])

            -- Helper for filtering for UTXOs containing a vote token
            hasVoteToken :: Value -> Bool
            hasVoteToken = hasSymbolInValue dcVoteCurrencySymbol

            -- Ensure there are no vote tokens in the outputs
            voteTokenAreAllBurned :: Bool
            !voteTokenAreAllBurned =
              not $ any (hasVoteToken . txOutValue) (txInfoOutputs :: [TxOut])
           in
            traceIfFalse "Transaction should be signed by the vote owner" isSignedByOwner
              && traceIfFalse "All vote tokens should be burned" voteTokenAreAllBurned

voteValidator :: ConfigurationValidatorConfig -> Validator
voteValidator config = mkValidatorWithSettings compiledCode False
  where
    wrapValidateVote = wrapValidate validateVote
    compiledCode = $$(PlutusTx.compile [||wrapValidateVote||]) `applyCode` liftCode config

voteValidatorHash :: ConfigurationValidatorConfig -> ValidatorHash
voteValidatorHash = validatorHash . voteValidator

voteScript :: ConfigurationValidatorConfig -> PlutusScript PlutusScriptV2
voteScript = validatorToScript voteValidator
