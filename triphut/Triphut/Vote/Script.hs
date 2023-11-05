module Triphut.Vote.Script (
  voteMinter,
  voteMinterPolicyId,
  mkVoteMinter,
  wrappedPolicy,
  voteScript,
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
  Script,
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
import Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasBurnedTokens,
  hasOneOfToken,
  hasSingleToken,
  hasSymbolInValue,
  hasTokenInValue,
  plutonomyMintingPolicyHash,
  validatorHash,
  wrapValidate,
 )
import Triphut.Types (TallyState (TallyState, tsProposalEndTime))
import Triphut.Vote (
  Vote (Vote, vOwner, vReturnAda),
  VoteAction (Cancel, Count),
  VoteAddress (vAddressCredential),
  VoteDynamicConfig (
    VoteDynamicConfig,
    vdcTallyValidator,
    vdcVoteCurrencySymbol
  ),
  VoteMinterAction (Burn, Mint),
  VoteMinterAddress (vmAddressCredential),
  VoteMinterConfig (
    VoteMinterConfig,
    vmcConfigNftCurrencySymbol,
    vmcConfigNftTokenName
  ),
  VoteMinterDynamicConfig (
    VoteMinterDynamicConfig,
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

{- | The vote minter has a reference to the proposal so the end time can be validated
   Ensures that there is an NFT for voting present
-}
mkVoteMinter :: VoteMinterConfig -> VoteMinterAction -> VoteMinterScriptContext -> Bool
mkVoteMinter
  VoteMinterConfig {..}
  action
  VoteMinterScriptContext
    { vmScriptContextTxInfo = VoteMinterTxInfo {..}
    , vmScriptContextPurpose = VMMinting thisCurrencySymbol
    } = case action of
    Burn ->
      let
        burnsTokens :: Bool
        !burnsTokens = hasBurnedTokens thisCurrencySymbol vmTxInfoMint "Vote Minter Burn"
       in
        traceIfFalse "Not burning tokens" burnsTokens
    Mint ->
      let
        hasConfigurationNft :: Value -> Bool
        hasConfigurationNft = hasOneOfToken vmcConfigNftCurrencySymbol vmcConfigNftTokenName

        theData :: Map DatumHash Datum
        theData = unsafeFromBuiltinData vmTxInfoData

        VoteMinterDynamicConfig {..} =
          case filter
            (hasConfigurationNft . vmTxOutValue . vmTxInInfoResolved)
            (unsafeFromBuiltinData vmTxInfoReferenceInputs) of
            [VoteMinterTxInInfo {vmTxInInfoResolved = VoteMinterTxOut {..}}] -> convertDatum theData vmTxOutDatum
            _ -> traceError "Too many NFT values"

        -- Get output on the vote validator. Should just be one.
        (Vote {..}, !voteValue) =
          case filter
            ((== ScriptCredential vmdcVoteValidator) . vmAddressCredential . vmTxOutAddress)
            (unsafeFromBuiltinData vmTxInfoOutputs) of
            [VoteMinterTxOut {..}] -> (convertDatum theData vmTxOutDatum, vmTxOutValue)
            _ -> traceError "Wrong number of proposal references"

        -- Find the reference input with the Tally nft currency symbol
        hasTallyNft :: Value -> Bool
        hasTallyNft = hasSymbolInValue vmdcTallyNft

        TallyState {..} =
          case filter
            (hasTallyNft . txOutValue . txInInfoResolved)
            (unsafeFromBuiltinData vmTxInfoReferenceInputs) of
            [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum theData txOutDatum
            _ -> traceError "Wrong number of tally references"

        proposalIsActive :: Bool
        !proposalIsActive = tsProposalEndTime `after` unsafeFromBuiltinData vmTxInfoValidRange

        hasWitness :: Bool
        !hasWitness = hasOneOfToken thisCurrencySymbol vmdcVoteTokenName voteValue

        onlyMintedOne :: Bool
        !onlyMintedOne = hasSingleToken vmTxInfoMint thisCurrencySymbol vmdcVoteTokenName

        hasVoteNft :: Bool
        !hasVoteNft = hasTokenInValue vmdcVoteNft "Vote NFT" voteValue

        totalAdaIsGreaterThanReturnAda :: Bool
        !totalAdaIsGreaterThanReturnAda = valueOf voteValue adaSymbol adaToken > vReturnAda
       in
        traceIfFalse "Proposal has expired" proposalIsActive
          && traceIfFalse "Vote Nft is missing" hasVoteNft
          && traceIfFalse "Missing witness on output" hasWitness
          && traceIfFalse "Wrong number of witnesses minted" onlyMintedOne
          && traceIfFalse "Total ada not high enough" totalAdaIsGreaterThanReturnAda

wrappedPolicy :: VoteMinterConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkVoteMinter config (unsafeFromBuiltinData a) (unsafeFromBuiltinData b))

policy :: VoteMinterConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

plutusScript :: VoteMinterConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: VoteMinterConfig -> Validator
validator = Validator . plutusScript

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
    serialise . Plutonomy.optimizeUPLCWith optimizerSettings . validator

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
  Vote ->
  VoteAction ->
  VoteScriptContext ->
  Bool
validateVote
  VoteValidatorConfig {..}
  Vote {..}
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

wrapValidateVote :: VoteValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidateVote = wrapValidate validateVote

voteValidator :: VoteValidatorConfig -> Validator
voteValidator cfg =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        , Plutonomy.ooFloatOutLambda = False
        }
   in
    Plutonomy.optimizeUPLCWith optimizerSettings $
      Plutonomy.validatorToPlutus $
        Plutonomy.mkValidatorScript $
          $$(PlutusTx.compile [||wrapValidateVote||])
            `applyCode` liftCode cfg

voteValidatorHash :: VoteValidatorConfig -> ValidatorHash
voteValidatorHash = validatorHash . voteValidator

voteScript :: VoteValidatorConfig -> PlutusScript PlutusScriptV2
voteScript =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . voteValidator
