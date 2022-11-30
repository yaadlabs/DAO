{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Canonical.ConfigurationNft where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Crypto
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           PlutusTx
import qualified PlutusTx.AssocMap as M
import           PlutusTx.Prelude
import qualified Plutonomy
import qualified Cardano.Api.Shelley as Shelly

type WrappedMintingPolicyType = BuiltinData -> BuiltinData -> ()

data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  , ncTokenName   :: TokenName
  }

data DynamicConfig = DynamicConfig
  { dcTallyIndexNft                 :: CurrencySymbol
  , dcTallyNft                      :: CurrencySymbol
  , dcUpgradeProposal               :: CurrencySymbol
  , dcUpgradeMajorityPercent        :: Integer
  , dcUpgradRelativeMajorityPercent :: Integer
  , dcTreasuryValidator             :: ValidatorHash
  , dcConfigurationValidator        :: ValidatorHash
  }

unstableMakeIsData ''DynamicConfig
makeLift ''NftConfig

extractDatumBytes :: [(DatumHash, Datum)] -> DatumHash -> BuiltinData
extractDatumBytes datums dh = getDatum $ extractDatum datums dh

extractDatum :: [(DatumHash, Datum)] -> DatumHash -> Datum
extractDatum datums dh = go datums where
  go = \case
    [] -> traceError "Failed to find datum"
    (x, y):xs ->
      if x == dh then
        y
      else
        go xs

hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken (Value v) s t = case M.lookup s v of
  Just m -> case M.toList m of
    [(t', c)] -> t' == t && c == 1
    _ -> traceError "wrong number of tokens with policy id"
  _ -> False

mkNftMinter :: NftConfig -> BuiltinData -> ScriptContext -> Bool
mkNftMinter NftConfig {..} _ ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } =
  let
    hasWitness :: Value -> Bool
    hasWitness (Value v) = case M.lookup thisCurrencySymbol v of
      Just m -> case M.toList m of
        [(_, c)] -> if c == 1 then True else traceError "wrong token count"
        _ -> traceError "wrong number of tokens with policy id"
      _ -> False

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == ncInitialUtxo) txInfoInputs

    -- This errors if more than one token is used as an output with this policy id
    _newOutput :: DynamicConfig
    _newOutput = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
      [ TxOut { txOutDatum }
        ] -> unsafeFromBuiltinData $ case txOutDatum of
          OutputDatum (Datum dbs) -> dbs
          OutputDatumHash dh0 -> extractDatumBytes (M.toList txInfoData) dh0
          NoOutputDatum -> traceError "Script output missing datum"
      _ -> traceError "Impossible. No minted output."

    onlyOneTokenMinted :: Bool
    onlyOneTokenMinted =
      hasSingleToken
        txInfoMint
        thisCurrencySymbol
        ncTokenName

  in traceIfFalse "Missing significant UTxO!" hasUTxO
  && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted

mkNftMinter _ _ _ = traceError "wrong type of script purpose!"

toCardanoApiScript :: Script -> Shelly.Script Shelly.PlutusScriptV2
toCardanoApiScript
  = Shelly.PlutusScript Shelly.PlutusScriptV2
  . Shelly.PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise

scriptHash :: Script -> ScriptHash
scriptHash =
    ScriptHash
    . toBuiltin
    . Shelly.serialiseToRawBytes
    . Shelly.hashScript
    . toCardanoApiScript

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash
  = MintingPolicyHash
  . getScriptHash
  . scriptHash
  . getValidator
  . Validator
  . getMintingPolicy

wrappedPolicy :: NftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkNftMinter config a (unsafeFromBuiltinData b))

policy :: NftConfig -> MintingPolicy
policy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicy c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

plutusScript :: NftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: NftConfig -> Validator
validator = Validator . plutusScript

nftMinterPolicyId :: NftConfig -> CurrencySymbol
nftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: NftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

nftMinter :: NftConfig -> PlutusScript PlutusScriptV2
nftMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . scriptAsCbor

-------------------------------------------------------------------------------
-- Validator
-- The validator makes sure the config cannot be changed unless an upgrade
-- request is present
-- The idea is that
-- I need to get a reference to a tally nft
-- That includes datum that has a reference utxo
-- That Utxo includes an upgrade proposal
-- The upgrade proposal has a datum that includes a minter
-- That if everything is good unlocks the treasury
-- and makes sure a token is minted to validate the upgrade
-------------------------------------------------------------------------------

data ConfigurationAddress = ConfigurationAddress
  { cAddressCredential        :: Credential
  , cAddressStakingCredential :: BuiltinData
  }

data ConfigurationTxOut = ConfigurationTxOut
  { cTxOutAddress             :: ConfigurationAddress
  , cTxOutValue               :: Value
  , cTxOutDatum               :: BuiltinData
  , cTxOutReferenceScript     :: BuiltinData
  }

data ConfigurationTxInInfo = ConfigurationTxInInfo
  { cTxInInfoOutRef   :: BuiltinData
  , cTxInInfoResolved :: ConfigurationTxOut
  }

data ConfigurationScriptContext = ConfigurationScriptContext
  { cScriptContextTxInfo  :: ConfigurationTxInfo
  , cScriptContextPurpose :: BuiltinData
  }

data ConfigurationTxInfo = ConfigurationTxInfo
  { cTxInfoInputs             :: [ConfigurationTxInInfo]
  , cTxInfoReferenceInputs    :: BuiltinData
  , cTxInfoOutputs            :: [ConfigurationTxOut]
  , cTxInfoFee                :: BuiltinData
  , cTxInfoMint               :: BuiltinData
  , cTxInfoDCert              :: BuiltinData
  , cTxInfoWdrl               :: BuiltinData
  , cTxInfoValidRange         :: BuiltinData
  , cTxInfoSignatories        :: [PubKeyHash]
  , cTxInfoRedeemers          :: BuiltinData
  , cTxInfoData               :: BuiltinData
  , cTxInfoId                 :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data ConfigurationAction
  = Upgrade

unstableMakeIsData ''ConfigurationAddress
unstableMakeIsData ''ConfigurationTxOut
unstableMakeIsData ''ConfigurationTxInInfo
unstableMakeIsData ''ConfigurationScriptContext
unstableMakeIsData ''ConfigurationTxInfo
unstableMakeIsData ''ConfigurationAction

validateConfiguration
  :: DynamicConfig
  -> ConfigurationAction
  -> ConfigurationScriptContext
  -> Bool
validateConfiguration
  DynamicConfig {} _
  ConfigurationScriptContext
    { cScriptContextTxInfo = ConfigurationTxInfo {}
    } = error ()

validatorHash :: Validator -> ValidatorHash
validatorHash = ValidatorHash . getScriptHash . scriptHash . getValidator

wrapValidateConfiguration
    :: BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateConfiguration x y z = check (
  validateConfiguration
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

configurationValidator :: Validator
configurationValidator = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateConfiguration ||])

configurationValidatorHash :: ValidatorHash
configurationValidatorHash = validatorHash configurationValidator

configurationScript :: PlutusScript PlutusScriptV2
configurationScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  $ serialise
    configurationValidator
