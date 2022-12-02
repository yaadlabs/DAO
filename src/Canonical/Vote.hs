module Canonical.Vote where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Tx
import           PlutusTx.AssocMap (Map)
import           PlutusTx
import           PlutusTx.Prelude
import           Canonical.Shared
import qualified Plutonomy


-- The vote minter
-- has a reference to the proposal so the end time can be validated

data VoteMinterConfig = VoteMinterConfig
  { vmcConfigNftCurrencySymbol :: CurrencySymbol
  , vmcConfigNftTokenName      :: TokenName
  }

makeLift ''VoteMinterConfig

data Vote = For | Against

unstableMakeIsData ''Vote

mkVoteMinter :: VoteMinterConfig -> BuiltinData -> ScriptContext -> Bool
mkVoteMinter VoteMinterConfig {} _ ScriptContext
  { scriptContextTxInfo = TxInfo {}
  , scriptContextPurpose = Minting _thisCurrencySymbol
  } = error ()

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
