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
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Tx
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
  }

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''Vote

-- | The vote minter
--   has a reference to the proposal so the end time can be validated
--   Ensures that there is an NFT for voting is present
mkVoteMinter :: VoteMinterConfig -> BuiltinData -> ScriptContext -> Bool
mkVoteMinter VoteMinterConfig {..} _ ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } =
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
    !onlyMintedOne = case M.toList (getValue txInfoMint) of
      [(p, m)]
        | p == thisCurrencySymbol -> case M.toList m of
          [(t, c)]
            -> traceIfFalse "Wrong number of witnesses minted" (c == 1)
            && traceIfFalse "Wrong token name" (t == dcVoteTokenName)
          _ -> traceError "Invalid tokens minted"
        | otherwise -> traceError "Invalid currency symbol minted"
      _ -> traceError "Wrong number of currency symbols minted"

    hasVoteNft :: Bool
    !hasVoteNft = case M.lookup dcVoteFungibleCurrencySymbol (getValue voteValue) of
      Nothing -> False
      Just m  -> case M.toList m of
          [(_, c)]
            -> traceIfFalse "Impossible. Vote NFT is not an NFT" (c == 1)
          _ -> traceError "Wrong number of vote NFTs"

  in traceIfFalse "Proposal has expired"             proposalIsActive
  && traceIfFalse "Vote Nft is missing"              hasVoteNft
  && traceIfFalse "Missing witness on output"        hasWitness
  && traceIfFalse "Wrong number of witnesses minted" onlyMintedOne

mkVoteMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: VoteMinterConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkVoteMinter config a (unsafeFromBuiltinData b))

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
  | Disburse

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
  VoteValidatorConfig {}
  _
  _
  VoteScriptContext
    { vScriptContextTxInfo = VoteTxInfo {}
    } = error ()

validatorHash :: Validator -> ValidatorHash
validatorHash = ValidatorHash . getScriptHash . scriptHash . getValidator

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
