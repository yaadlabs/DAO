module Canonical.Vote where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V1.Ledger.Address
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Tx hiding (Mint)
import           PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import           PlutusTx
import           PlutusTx.Prelude
import           Canonical.Shared
import           Canonical.Types
import qualified Plutonomy

data VoteMinterConfig = VoteMinterConfig
  { vmcConfigNftCurrencySymbol :: CurrencySymbol
  , vmcConfigNftTokenName      :: TokenName
  }

makeLift ''VoteMinterConfig

data VoteDirection = For | Against

data Vote = Vote
  { vProposal  :: TxOutRef
  , vDirection :: VoteDirection
  , vCounted   :: Bool
  , vOwner     :: Address
  , vReturnAda :: Integer
  }

data VoteMinterAction = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterAction
unstableMakeIsData ''Vote

-- | The vote minter
--   has a reference to the proposal so the end time can be validated
--   Ensures that there is an NFT for voting is present
mkVoteMinter :: VoteMinterConfig -> VoteMinterAction -> ScriptContext -> Bool
mkVoteMinter VoteMinterConfig {..} action ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } = case action of
    Burn ->
      let
        burnsTokens :: Bool
        !burnsTokens = case M.lookup thisCurrencySymbol (getValue txInfoMint) of
          Nothing -> traceError "Impossible. Vote minter called but no vote tokens are minted"
          Just m -> case M.toList m of
            [(_, c)] -> traceIfFalse "Count is not less than zero" (c < 0)
            _ -> traceError "Wrong number of tokens"

      in traceIfFalse "Not burning tokens" burnsTokens
    Mint ->
      let
        hasConfigurationNft :: Value -> Bool
        hasConfigurationNft (Value v) = case M.lookup vmcConfigNftCurrencySymbol v of
          Nothing -> False
          Just m  -> case M.lookup vmcConfigNftTokenName m of
            Nothing -> False
            Just c -> c == 1

        DynamicConfig {..} = case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Too many NFT values"

        -- Get output on the vote validator. Should just be one.
        (Vote {..}, !voteValue) = case filter ((==ScriptCredential dcVoteValidator) . addressCredential . txOutAddress) txInfoOutputs of
          [TxOut {..}] -> (convertDatum txInfoData txOutDatum, txOutValue)
          _ -> traceError "Wrong number of proposal references"

        Proposal {..} = case filter ((==vProposal) . txInInfoOutRef) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Wrong number of proposal references"

        proposalIsActive :: Bool
        !proposalIsActive = pEndTime `after` txInfoValidRange

        hasWitness :: Bool
        !hasWitness = case M.lookup thisCurrencySymbol (getValue voteValue) of
          Nothing -> False
          Just m  -> case M.lookup dcVoteTokenName m of
            Nothing -> False
            Just c -> c == 1

        onlyMintedOne :: Bool
        !onlyMintedOne = case M.lookup thisCurrencySymbol (getValue txInfoMint) of
          Nothing -> traceError "Nothing of this currency symbol minted"
          Just m -> case M.toList m of
            [(t, c)]
              -> traceIfFalse "Wrong number of witnesses minted" (c == 1)
              && traceIfFalse "Wrong token name" (t == dcVoteTokenName)
            _ -> traceError "Invalid tokens minted"

        hasVoteNft :: Bool
        !hasVoteNft = case M.lookup dcVoteNft (getValue voteValue) of
          Nothing -> False
          Just m  -> case M.toList m of
              [(_, c)]
                -> traceIfFalse "Impossible. Vote NFT is not an NFT" (c == 1)
              _ -> traceError "Wrong number of vote NFTs"

        voteIsNotCounted :: Bool
        !voteIsNotCounted = not vCounted

        totalAdaIsGreaterThanReturnAda :: Bool
        !totalAdaIsGreaterThanReturnAda = valueOf voteValue adaSymbol adaToken > vReturnAda

      in traceIfFalse "Proposal has expired"             proposalIsActive
      && traceIfFalse "Vote Nft is missing"              hasVoteNft
      && traceIfFalse "Missing witness on output"        hasWitness
      && traceIfFalse "Wrong number of witnesses minted" onlyMintedOne
      && traceIfFalse "Vote is counted"                  voteIsNotCounted
      && traceIfFalse "Total ada not high enough"        totalAdaIsGreaterThanReturnAda

mkVoteMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: VoteMinterConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkVoteMinter config (unsafeFromBuiltinData a) (unsafeFromBuiltinData b))

policy :: VoteMinterConfig -> MintingPolicy
policy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicy c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

plutusScript :: VoteMinterConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: VoteMinterConfig -> Validator
validator = Validator . plutusScript

voteMinterPolicyId :: VoteMinterConfig -> CurrencySymbol
voteMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: VoteMinterConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

voteMinter :: VoteMinterConfig -> PlutusScript PlutusScriptV2
voteMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . scriptAsCbor

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data VoteAddress = VoteAddress
  { vAddressCredential        :: Credential
  , vAddressStakingCredential :: BuiltinData
  }

data VoteTxOut = VoteTxOut
  { vTxOutAddress             :: VoteAddress
  , vTxOutValue               :: Value
  , vTxOutDatum               :: OutputDatum
  , vTxOutReferenceScript     :: BuiltinData
  }

data VoteTxInInfo = VoteTxInInfo
  { vTxInInfoOutRef   :: TxOutRef
  , vTxInInfoResolved :: VoteTxOut
  }

data VoteScriptPurpose = VoteSpend TxOutRef

data VoteScriptContext = VoteScriptContext
  { vScriptContextTxInfo  :: VoteTxInfo
  , vScriptContextPurpose :: VoteScriptPurpose
  }

data VoteTxInfo = VoteTxInfo
  { vTxInfoInputs             :: [VoteTxInInfo]
  , vTxInfoReferenceInputs    :: [VoteTxInInfo]
  , vTxInfoOutputs            :: [VoteTxOut]
  , vTxInfoFee                :: BuiltinData
  , vTxInfoMint               :: BuiltinData
  , vTxInfoDCert              :: BuiltinData
  , vTxInfoWdrl               :: BuiltinData
  , vTxInfoValidRange         :: BuiltinData
  , vTxInfoSignatories        :: [PubKeyHash]
  , vTxInfoRedeemers          :: BuiltinData
  , vTxInfoData               :: Map DatumHash Datum
  , vTxInfoId                 :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data VoteAction
  = Count
  | Cancel

data VoteValidatorConfig = VoteValidatorConfig
  { vvcConfigNftCurrencySymbol :: CurrencySymbol
  , vvcConfigNftTokenName      :: TokenName
  }

unstableMakeIsData ''VoteAddress
unstableMakeIsData ''VoteTxOut
unstableMakeIsData ''VoteTxInInfo
makeIsDataIndexed  ''VoteScriptPurpose [('VoteSpend,1)]
unstableMakeIsData ''VoteScriptContext
unstableMakeIsData ''VoteTxInfo
unstableMakeIsData ''VoteAction
makeLift ''VoteValidatorConfig

-- Needs to work in bulk
validateVote
  :: VoteValidatorConfig
  -> Vote
  -> VoteAction
  -> VoteScriptContext
  -> Bool
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
      Just m  -> case M.lookup vvcConfigNftTokenName m of
        Nothing -> False
        Just c -> c == 1

    DynamicConfig {..} = case filter (hasConfigurationNft . vTxOutValue . vTxInInfoResolved) vTxInfoReferenceInputs of
      [VoteTxInInfo {vTxInInfoResolved = VoteTxOut {..}}] -> convertDatum vTxInfoData vTxOutDatum
      _ -> traceError "Too many NFT values"

  in case action of
    Count ->
      traceIfFalse
          "Missing Tally Validator input"
          (any
            ( (== ScriptCredential dcTallyValidator)
            . vAddressCredential
            . vTxOutAddress
            . vTxInInfoResolved
            )
            vTxInfoInputs
          )
    Cancel ->
      let
        isSignedByOwner :: Bool
        !isSignedByOwner = any ((== addressCredential vOwner) . PubKeyCredential) vTxInfoSignatories

        hasVoteToken :: Value -> Bool
        hasVoteToken (Value v) = case M.lookup dcVoteCurrencySymbol v of
          Nothing -> False
          Just _ -> True

        voteTokenAreAllBurned :: Bool
        !voteTokenAreAllBurned = not $ any (hasVoteToken . vTxOutValue) vTxInfoOutputs

        isAvailableForCancel :: Bool
        !isAvailableForCancel =
          if vCounted then
            let
              Proposal {..} = case filter ((==vProposal) . vTxInInfoOutRef) vTxInfoReferenceInputs of
                [VoteTxInInfo {vTxInInfoResolved = VoteTxOut {..}}] -> convertDatum vTxInfoData vTxOutDatum
                _ -> traceError "Wrong number of proposal references"

              isProposalActive :: Bool
              !isProposalActive = (pEndTime + POSIXTime dcProposalTallyEndOffset) `before` unsafeFromBuiltinData vTxInfoValidRange

            in isProposalActive
          else
            True

      in traceIfFalse "Not signed by owner" isSignedByOwner
      && traceIfFalse "All vote tokens are not burned" voteTokenAreAllBurned
      && traceIfFalse "Is not available for cancelling yet" isAvailableForCancel


wrapValidateVote
    :: VoteValidatorConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateVote cfg x y z = check (
  validateVote
    cfg
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

voteValidator :: VoteValidatorConfig -> Validator
voteValidator cfg = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateVote ||])
    `applyCode`
    liftCode cfg

voteValidatorHash :: VoteValidatorConfig -> ValidatorHash
voteValidatorHash = validatorHash . voteValidator

voteScript :: VoteValidatorConfig ->  PlutusScript PlutusScriptV2
voteScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . voteValidator
