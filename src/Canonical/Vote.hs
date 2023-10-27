module Canonical.Vote (
  Vote (..),
  VoteDirection (..),
  VoteMinterConfig (..),
  VoteValidatorConfig (..),
  voteScript,
  voteMinter,
  voteMinterPolicyId,
  voteValidatorHash,
) where

import Canonical.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  plutonomyMintingPolicyHash,
  validatorHash,
 )
import Canonical.Types (TallyState (TallyState, tsProposalEndTime))
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Plutonomy
import Plutus.V1.Ledger.Address (Address, addressCredential)
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
  TokenName,
  Value (Value),
  adaSymbol,
  adaToken,
  getValue,
  mpsSymbol,
  valueOf,
 )
import Plutus.V2.Ledger.Contexts (TxInInfo (TxInInfo, txInInfoResolved))
import Plutus.V2.Ledger.Tx hiding (Mint)
import PlutusTx (applyCode, compile, liftCode, makeLift, unsafeFromBuiltinData, unstableMakeIsData)
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Eq,
  Integer,
  Maybe (Just, Nothing),
  any,
  check,
  filter,
  not,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
  (<),
  (==),
  (>),
 )

data VoteMinterConfig = VoteMinterConfig
  { vmcConfigNftCurrencySymbol :: CurrencySymbol
  , vmcConfigNftTokenName :: TokenName
  }

makeLift ''VoteMinterConfig

data VoteDirection = For | Against

instance Eq VoteDirection where
  x == y = case (x, y) of
    (For, For) -> True
    (Against, Against) -> True
    _ -> False

data Vote = Vote
  { vProposalTokenName :: TokenName
  , vDirection :: VoteDirection
  , vOwner :: Address
  , vReturnAda :: Integer
  }

data VoteMinterAction = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterAction
unstableMakeIsData ''Vote

data VoteMinterAddress = VoteMinterAddress
  { vmAddressCredential :: Credential
  , vmAddressStakingCredential :: BuiltinData
  }

data VoteMinterTxOut = VoteMinterTxOut
  { vmTxOutAddress :: VoteMinterAddress
  , vmTxOutValue :: Value
  , vmTxOutDatum :: OutputDatum
  , vmTxOutReferenceScript :: BuiltinData
  }

data VoteMinterTxInInfo = VoteMinterTxInInfo
  { vmTxInInfoOutRef :: BuiltinData
  , vmTxInInfoResolved :: VoteMinterTxOut
  }

data VoteMinterScriptPurpose = VMMinting CurrencySymbol

data VoteMinterScriptContext = VoteMinterScriptContext
  { vmScriptContextTxInfo :: VoteMinterTxInfo
  , vmScriptContextPurpose :: VoteMinterScriptPurpose
  }

data VoteMinterTxInfo = VoteMinterTxInfo
  { vmTxInfoInputs :: BuiltinData
  , vmTxInfoReferenceInputs :: BuiltinData
  , vmTxInfoOutputs :: BuiltinData
  , vmTxInfoFee :: BuiltinData
  , vmTxInfoMint :: Value
  , vmTxInfoDCert :: BuiltinData
  , vmTxInfoWdrl :: BuiltinData
  , vmTxInfoValidRange :: BuiltinData
  , vmTxInfoSignatories :: BuiltinData
  , vmTxInfoRedeemers :: BuiltinData
  , vmTxInfoData :: BuiltinData
  , vmTxInfoId :: BuiltinData
  }

data VoteMinterDynamicConfig = VoteMinterDynamicConfig
  { vmdcTallyIndexNft :: BuiltinData
  , vmdcTallyNft :: CurrencySymbol
  , vmdcTallyValidator :: BuiltinData
  , vmdcTreasuryValidator :: BuiltinData
  , vmdcConfigurationValidator :: BuiltinData
  , vmdcVoteCurrencySymbol :: BuiltinData
  , vmdcVoteTokenName :: TokenName
  , vmdcVoteValidator :: ValidatorHash
  , vmdcUpgradeMajorityPercent :: BuiltinData
  , vmdcUpgradRelativeMajorityPercent :: BuiltinData
  , vmdcGeneralMajorityPercent :: BuiltinData
  , vmdcGeneralRelativeMajorityPercent :: BuiltinData
  , vmdcTripMajorityPercent :: BuiltinData
  , vmdcTripRelativeMajorityPercent :: BuiltinData
  , vmdcTotalVotes :: BuiltinData
  , vmdcVoteNft :: CurrencySymbol
  , vmdcVoteFungibleCurrencySymbol :: BuiltinData
  , vmdcVoteFungibleTokenName :: BuiltinData
  , vmdcProposalTallyEndOffset :: BuiltinData
  , vmdcMaxGeneralDisbursement :: BuiltinData
  , vmdcMaxTripDisbursement :: BuiltinData
  , vmdcAgentDisbursementPercent :: BuiltinData
  , vmdcFungibleVotePercent :: BuiltinData
  }

unstableMakeIsData ''VoteMinterAddress
unstableMakeIsData ''VoteMinterTxOut
unstableMakeIsData ''VoteMinterTxInInfo
unstableMakeIsData ''VoteMinterScriptPurpose
unstableMakeIsData ''VoteMinterScriptContext
unstableMakeIsData ''VoteMinterTxInfo
unstableMakeIsData ''VoteMinterDynamicConfig

{- | The vote minter
   has a reference to the proposal so the end time can be validated
   Ensures that there is an NFT for voting is present
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
        !burnsTokens = case M.lookup thisCurrencySymbol (getValue vmTxInfoMint) of
          Nothing -> traceError "Impossible. Vote minter called but no vote tokens are minted"
          Just m -> case M.toList m of
            [(_, c)] -> traceIfFalse "Count is not less than zero" (c < 0)
            _ -> traceError "Wrong number of tokens"
       in
        traceIfFalse "Not burning tokens" burnsTokens
    Mint ->
      let
        hasConfigurationNft :: Value -> Bool
        hasConfigurationNft (Value v) = case M.lookup vmcConfigNftCurrencySymbol v of
          Nothing -> False
          Just m -> case M.lookup vmcConfigNftTokenName m of
            Nothing -> False
            Just c -> c == 1

        theData :: Map DatumHash Datum
        theData = unsafeFromBuiltinData vmTxInfoData

        VoteMinterDynamicConfig {..} = case filter (hasConfigurationNft . vmTxOutValue . vmTxInInfoResolved) (unsafeFromBuiltinData vmTxInfoReferenceInputs) of
          [VoteMinterTxInInfo {vmTxInInfoResolved = VoteMinterTxOut {..}}] -> convertDatum theData vmTxOutDatum
          _ -> traceError "Too many NFT values"

        -- Get output on the vote validator. Should just be one.
        (Vote {..}, !voteValue) = case filter ((== ScriptCredential vmdcVoteValidator) . vmAddressCredential . vmTxOutAddress) (unsafeFromBuiltinData vmTxInfoOutputs) of
          [VoteMinterTxOut {..}] -> (convertDatum theData vmTxOutDatum, vmTxOutValue)
          _ -> traceError "Wrong number of proposal references"

        -- Find the reference input with the Tally nft currency symbol
        hasTallyNft :: Value -> Bool
        hasTallyNft (Value v) = case M.lookup vmdcTallyNft v of
          Nothing -> False
          Just _ -> True

        TallyState {..} = case filter (hasTallyNft . txOutValue . txInInfoResolved) (unsafeFromBuiltinData vmTxInfoReferenceInputs) of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum theData txOutDatum
          _ -> traceError "Wrong number of tally references"

        proposalIsActive :: Bool
        !proposalIsActive = tsProposalEndTime `after` (unsafeFromBuiltinData vmTxInfoValidRange)

        hasWitness :: Bool
        !hasWitness = case M.lookup thisCurrencySymbol (getValue voteValue) of
          Nothing -> False
          Just m -> case M.lookup vmdcVoteTokenName m of
            Nothing -> False
            Just c -> c == 1

        onlyMintedOne :: Bool
        !onlyMintedOne = case M.lookup thisCurrencySymbol (getValue vmTxInfoMint) of
          Nothing -> traceError "Nothing of this currency symbol minted"
          Just m -> case M.toList m of
            [(t, c)] ->
              traceIfFalse "Wrong number of witnesses minted" (c == 1)
                && traceIfFalse "Wrong token name" (t == vmdcVoteTokenName)
            _ -> traceError "Invalid tokens minted"

        hasVoteNft :: Bool
        !hasVoteNft = case M.lookup vmdcVoteNft (getValue voteValue) of
          Nothing -> False
          Just m -> case M.toList m of
            [(_, c)] ->
              traceIfFalse "Impossible. Vote NFT is not an NFT" (c == 1)
            _ -> traceError "Wrong number of vote NFTs"

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

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data VoteAddress = VoteAddress
  { vAddressCredential :: Credential
  , vAddressStakingCredential :: BuiltinData
  }

data VoteTxOut = VoteTxOut
  { vTxOutAddress :: VoteAddress
  , vTxOutValue :: Value
  , vTxOutDatum :: OutputDatum
  , vTxOutReferenceScript :: BuiltinData
  }

data VoteTxInInfo = VoteTxInInfo
  { vTxInInfoOutRef :: TxOutRef
  , vTxInInfoResolved :: VoteTxOut
  }

data VoteScriptContext = VoteScriptContext
  { vScriptContextTxInfo :: VoteTxInfo
  , vScriptContextPurpose :: BuiltinData
  }

data VoteTxInfo = VoteTxInfo
  { vTxInfoInputs :: BuiltinData
  , vTxInfoReferenceInputs :: [VoteTxInInfo]
  , vTxInfoOutputs :: BuiltinData
  , vTxInfoFee :: BuiltinData
  , vTxInfoMint :: BuiltinData
  , vTxInfoDCert :: BuiltinData
  , vTxInfoWdrl :: BuiltinData
  , vTxInfoValidRange :: BuiltinData
  , vTxInfoSignatories :: BuiltinData
  , vTxInfoRedeemers :: BuiltinData
  , vTxInfoData :: Map DatumHash Datum
  , vTxInfoId :: BuiltinData
  }

data VoteDynamicConfig = VoteDynamicConfig
  { vdcTallyIndexNft :: BuiltinData
  , vdcTallyNft :: BuiltinData
  , vdcTallyValidator :: BuiltinData
  , vdcTreasuryValidator :: BuiltinData
  , vdcConfigurationValidator :: BuiltinData
  , vdcVoteCurrencySymbol :: BuiltinData
  , vdcVoteTokenName :: BuiltinData
  , vdcVoteValidator :: BuiltinData
  , vdcUpgradeMajorityPercent :: BuiltinData
  , vdcUpgradRelativeMajorityPercent :: BuiltinData
  , vdcGeneralMajorityPercent :: BuiltinData
  , vdcGeneralRelativeMajorityPercent :: BuiltinData
  , vdcTripMajorityPercent :: BuiltinData
  , vdcTripRelativeMajorityPercent :: BuiltinData
  , vdcTotalVotes :: BuiltinData
  , vdcVoteNft :: BuiltinData
  , vdcVoteFungibleCurrencySymbol :: BuiltinData
  , vdcVoteFungibleTokenName :: BuiltinData
  , vdcProposalTallyEndOffset :: BuiltinData
  , vdcMaxGeneralDisbursement :: BuiltinData
  , vdcMaxTripDisbursement :: BuiltinData
  , vdcAgentDisbursementPercent :: BuiltinData
  , vdcFungibleVotePercent :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data VoteAction
  = Count
  | Cancel

data VoteValidatorConfig = VoteValidatorConfig
  { vvcConfigNftCurrencySymbol :: CurrencySymbol
  , vvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''VoteAddress
unstableMakeIsData ''VoteTxOut
unstableMakeIsData ''VoteTxInInfo
unstableMakeIsData ''VoteScriptContext
unstableMakeIsData ''VoteTxInfo
unstableMakeIsData ''VoteAction
unstableMakeIsData ''VoteDynamicConfig
makeLift ''VoteValidatorConfig

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
      hasConfigurationNft (Value v) = case M.lookup vvcConfigNftCurrencySymbol v of
        Nothing -> False
        Just m -> case M.lookup vvcConfigNftTokenName m of
          Nothing -> False
          Just c -> c == 1

      VoteDynamicConfig {..} = case filter (hasConfigurationNft . vTxOutValue . vTxInInfoResolved) vTxInfoReferenceInputs of
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
            !isSignedByOwner = any ((== addressCredential vOwner) . PubKeyCredential) (unsafeFromBuiltinData vTxInfoSignatories :: [PubKeyHash])

            hasVoteToken :: Value -> Bool
            hasVoteToken (Value v) = case M.lookup (unsafeFromBuiltinData vdcVoteCurrencySymbol) v of
              Nothing -> False
              Just _ -> True

            voteTokenAreAllBurned :: Bool
            !voteTokenAreAllBurned = not $ any (hasVoteToken . vTxOutValue) (unsafeFromBuiltinData vTxInfoOutputs :: [VoteTxOut])
           in
            traceIfFalse "Not signed by owner" isSignedByOwner
              && traceIfFalse "All vote tokens are not burned" voteTokenAreAllBurned

wrapValidateVote ::
  VoteValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidateVote cfg x y z =
  check
    ( validateVote
        cfg
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

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
