{- |
Module: Triphut.Vote.Treasury
Description: Triphut vote related scripts.
Includes:
  - Vote minting policy script.
  - Vote validator script.
-}
module Triphut.Vote.Script (
  voteMinter,
  voteMinterPolicyId,
  mkVoteMinter,
  wrappedPolicy,
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
import Plutus.V2.Ledger.Contexts (TxInInfo (TxInInfo, txInInfoResolved))
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
import Triphut.Types (TallyStateDatum (TallyStateDatum, tsProposalEndTime))
import Triphut.Vote (
  VoteAction (Cancel, Count),
  VoteAddress (vAddressCredential),
  VoteDatum (VoteDatum, vOwner, vReturnAda),
  VoteDynamicConfig (
    VoteDynamicConfig,
    vdcTallyValidator,
    vdcVoteCurrencySymbol
  ),
  VoteMinterActionRedeemer (Burn, Mint),
  VoteMinterAddress (vmAddressCredential),
  VoteMinterConfig (
    VoteMinterConfig,
    vmcConfigNftCurrencySymbol,
    vmcConfigNftTokenName
  ),
  VoteMinterDynamicConfigDatum (
    VoteMinterDynamicConfigDatum,
    vmdcTallyNft,
    vmdcVoteNft,
    vmdcVoteTokenName,
    vmdcVoteValidator
  ),
  VoteMinterScriptContext (
    VoteMinterScriptContext,
    vmScriptContextPurpose,
    vmScriptContextTxInfo
  ),
  VoteMinterScriptPurpose (VMMinting),
  VoteMinterTxInInfo (
    VoteMinterTxInInfo,
    vmTxInInfoResolved
  ),
  VoteMinterTxInfo (
    VoteMinterTxInfo,
    vmTxInfoData,
    vmTxInfoMint,
    vmTxInfoOutputs,
    vmTxInfoReferenceInputs,
    vmTxInfoValidRange
  ),
  VoteMinterTxOut (
    VoteMinterTxOut,
    vmTxOutAddress,
    vmTxOutDatum,
    vmTxOutValue
  ),
  VoteScriptContext (
    VoteScriptContext,
    vScriptContextTxInfo
  ),
  VoteTxInInfo (
    VoteTxInInfo,
    vTxInInfoResolved
  ),
  VoteTxInfo (
    VoteTxInfo,
    vTxInfoData,
    vTxInfoInputs,
    vTxInfoOutputs,
    vTxInfoReferenceInputs,
    vTxInfoSignatories
  ),
  VoteTxOut (
    VoteTxOut,
    vTxOutAddress,
    vTxOutDatum,
    vTxOutValue
  ),
  VoteValidatorConfig (
    VoteValidatorConfig,
    vvcConfigNftCurrencySymbol,
    vvcConfigNftTokenName
  ),
 )

{- | Policy for minting or burning the vote NFT.

   == Minting Vote Token

      When the 'Triphut.Vote.VoteMinterActionRedeemer' redeemer
      is set to 'Mint', this policy performs the following checks:

        - There is exactly one 'Triphut.Vote.VoteMinterDynamicConfigDatum' in the reference inputs,
          marked by the config NFT
          (Corresponding config 'CurrencySymbol' and 'TokenName' provided by the 'VoteMinterConfig' argument)
        - There is exactly one 'Triphut.Types.TallyStateDatum' in the reference inputs,
          marked by the Tally NFT
        - Exactly one valid Vote NFT is minted with the valid token name.
        - The token name matches the 'vmdcVoteTokenName' field of the 'VoteMinterDynamicConfigDatum'
        - There is exactly one output containing the vote NFT.
        - This output contains a valid 'Triphut.Vote.VoteDatum' datum.
        - The proposal is still active.
          Checked by ensuring the proposal end time provided by
          the 'TallyStateDatum' isafter the validity range of the transaction
        - The total ada is greater than the return ada specificed by the 'vReturnAda' field of the 'VoteDatum'

   == Burning Vote Token

      When the 'Triphut.Vote.VoteMinterActionRedeemer' redeemer
      is set to 'Burn', this policy performs the following checks:

        - That one vote token is burned
-}
mkVoteMinter :: VoteMinterConfig -> VoteMinterActionRedeemer -> VoteMinterScriptContext -> Bool
mkVoteMinter
  VoteMinterConfig {..}
  action
  VoteMinterScriptContext
    { vmScriptContextTxInfo = VoteMinterTxInfo {..}
    , vmScriptContextPurpose = VMMinting thisCurrencySymbol
    } = case action of
    Burn ->
      let
        -- Check the transaction burns a valid token
        burnsTokens :: Bool
        !burnsTokens = hasBurnedTokens thisCurrencySymbol vmTxInfoMint "Vote Minter Burn"
       in
        traceIfFalse "Need to burn a vote token" burnsTokens
    Mint ->
      let
        -- Helper for filtering for config UTXO in the reference inputs
        hasConfigurationNft :: Value -> Bool
        hasConfigurationNft = hasOneOfToken vmcConfigNftCurrencySymbol vmcConfigNftTokenName

        -- The datums
        theData :: Map DatumHash Datum
        theData = unsafeFromBuiltinData vmTxInfoData

        -- Get the configuration from the reference inputs
        VoteMinterDynamicConfigDatum {..} =
          case filter
            (hasConfigurationNft . vmTxOutValue . vmTxInInfoResolved)
            (unsafeFromBuiltinData vmTxInfoReferenceInputs) of
            [VoteMinterTxInInfo {vmTxInInfoResolved = VoteMinterTxOut {..}}] -> convertDatum theData vmTxOutDatum
            _ -> traceError "Should be exactly one valid config in the reference inputs"

        -- Get output at the vote validator, should just be one.
        (VoteDatum {..}, !voteValue) =
          case filter
            ((== ScriptCredential vmdcVoteValidator) . vmAddressCredential . vmTxOutAddress)
            (unsafeFromBuiltinData vmTxInfoOutputs) of
            [VoteMinterTxOut {..}] -> (convertDatum theData vmTxOutDatum, vmTxOutValue)
            _ -> traceError "Should be exactly one vote datum (proposal reference) at the output"

        -- Helper for filtering for tally UTXO in the outputs
        hasTallyNft :: Value -> Bool
        hasTallyNft = hasSymbolInValue vmdcTallyNft

        -- Get the tally state datum at the output marked by the tally NFT
        TallyStateDatum {tsProposalEndTime} =
          case filter
            (hasTallyNft . txOutValue . txInInfoResolved)
            (unsafeFromBuiltinData vmTxInfoReferenceInputs) of
            [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum theData txOutDatum
            _ -> traceError "Should be exactly one tally state datum in the reference inputs"

        -- Ensure the proposal end time is after the transaction's validity range
        proposalIsActive :: Bool
        !proposalIsActive = tsProposalEndTime `after` unsafeFromBuiltinData vmTxInfoValidRange

        -- Ensure the vote value contains exactly one valid witness token
        hasWitness :: Bool
        !hasWitness = hasOneOfToken thisCurrencySymbol vmdcVoteTokenName voteValue

        -- Ensure exactly one valid vote token is minted
        onlyMintedOne :: Bool
        !onlyMintedOne =
          hasSingleTokenWithSymbolAndTokenName
            vmTxInfoMint
            thisCurrencySymbol
            vmdcVoteTokenName

        hasVoteNft :: Bool
        !hasVoteNft = hasTokenInValue vmdcVoteNft "Vote NFT" voteValue

        -- Ensure the return ADA is less than the ada contained in the vote value
        totalAdaIsGreaterThanReturnAda :: Bool
        !totalAdaIsGreaterThanReturnAda = valueOf voteValue adaSymbol adaToken > vReturnAda
       in
        traceIfFalse "Proposal has expired" proposalIsActive
          && traceIfFalse "Vote Nft is missing" hasVoteNft
          && traceIfFalse "Missing witness on output" hasWitness
          && traceIfFalse "Should be exactly one valid token minted" onlyMintedOne
          && traceIfFalse "Total ada is not high enough" totalAdaIsGreaterThanReturnAda

wrappedPolicy :: VoteMinterConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkVoteMinter config (unsafeFromBuiltinData a) (unsafeFromBuiltinData b))

policy :: VoteMinterConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

voteMinterPolicyId :: VoteMinterConfig -> CurrencySymbol
voteMinterPolicyId = mpsSymbol . plutonomyMintingPolicyHash . policy

scriptAsCbor :: VoteMinterConfig -> BSL.ByteString
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

voteMinter :: VoteMinterConfig -> PlutusScript PlutusScriptV2
voteMinter =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . scriptAsCbor

-- | Validator

-- Needs to work in bulk
validateVote ::
  VoteValidatorConfig ->
  VoteDatum ->
  VoteAction ->
  VoteScriptContext ->
  Bool
validateVote
  VoteValidatorConfig {..}
  VoteDatum {..}
  action
  VoteScriptContext
    { vScriptContextTxInfo = VoteTxInfo {..}
    } =
    let
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken vvcConfigNftCurrencySymbol vvcConfigNftTokenName

      VoteDynamicConfig {..} =
        case filter (hasConfigurationNft . vTxOutValue . vTxInInfoResolved) vTxInfoReferenceInputs of
          [VoteTxInInfo {vTxInInfoResolved = VoteTxOut {..}}] -> convertDatum vTxInfoData vTxOutDatum
          _ -> traceError "Too many NFT values"
     in
      case action of
        Count ->
          traceIfFalse
            "Missing Tally Validator input"
            ( any
                ( (== ScriptCredential (unsafeFromBuiltinData vdcTallyValidator))
                    . vAddressCredential
                    . vTxOutAddress
                    . vTxInInfoResolved
                )
                (unsafeFromBuiltinData vTxInfoInputs :: [VoteTxInInfo])
            )
        Cancel ->
          let
            isSignedByOwner :: Bool
            !isSignedByOwner =
              any
                ((== addressCredential vOwner) . PubKeyCredential)
                (unsafeFromBuiltinData vTxInfoSignatories :: [PubKeyHash])

            hasVoteToken :: Value -> Bool
            hasVoteToken = hasSymbolInValue (unsafeFromBuiltinData vdcVoteCurrencySymbol)

            voteTokenAreAllBurned :: Bool
            !voteTokenAreAllBurned =
              not $ any (hasVoteToken . vTxOutValue) (unsafeFromBuiltinData vTxInfoOutputs :: [VoteTxOut])
           in
            traceIfFalse "Not signed by owner" isSignedByOwner
              && traceIfFalse "All vote tokens are not burned" voteTokenAreAllBurned

voteValidator :: VoteValidatorConfig -> Validator
voteValidator config = mkValidatorWithSettings compiledCode False
  where
    wrapValidateVote = wrapValidate validateVote
    compiledCode = $$(PlutusTx.compile [||wrapValidateVote||]) `applyCode` liftCode config

voteValidatorHash :: VoteValidatorConfig -> ValidatorHash
voteValidatorHash = validatorHash . voteValidator

voteScript :: VoteValidatorConfig -> PlutusScript PlutusScriptV2
voteScript = validatorToScript voteValidator
