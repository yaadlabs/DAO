module Canonical.ConfigurationNft (
  ConfigurationValidatorConfig (..),
  NftConfig (..),
  configurationScript,
  configurationValidatorHash,
  nftMinter,
  nftMinterPolicyId,
) where

import Canonical.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasOneOfToken,
  hasSingleToken,
  hasSymbolInValue,
  hasTokenInValue,
  mintingPolicyHash,
  validatorHash,
 )
import Canonical.Types (
  DynamicConfig (
    DynamicConfig,
    dcProposalTallyEndOffset,
    dcTallyNft,
    dcTotalVotes,
    dcUpgradRelativeMajorityPercent,
    dcUpgradeMajorityPercent
  ),
  ProposalType (Upgrade),
  TallyState (TallyState, tsAgainst, tsFor, tsProposal, tsProposalEndTime),
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Credential (Credential)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval (before)
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
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime), POSIXTimeRange)
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName,
  Value,
  mpsSymbol,
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInInfo (txInInfoOutRef),
  TxInfo (TxInfo, txInfoData, txInfoInputs, txInfoMint, txInfoOutputs),
 )
import Plutus.V2.Ledger.Tx (
  OutputDatum,
  TxOut (TxOut, txOutDatum, txOutValue),
  TxOutRef,
 )
import PlutusTx (
  applyCode,
  compile,
  liftCode,
  makeIsDataIndexed,
  makeLift,
  unsafeFromBuiltinData,
  unstableMakeIsData,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (
  Bool (False),
  BuiltinData,
  Integer,
  any,
  check,
  divide,
  filter,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (*),
  (+),
  (.),
  (==),
  (>=),
 )

data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  , ncTokenName :: TokenName
  }

makeLift ''NftConfig

mkNftMinter :: NftConfig -> BuiltinData -> ScriptContext -> Bool
mkNftMinter
  NftConfig {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      hasWitness :: Value -> Bool
      hasWitness = hasTokenInValue thisCurrencySymbol "Configuration Witness"

      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == ncInitialUtxo) txInfoInputs

      -- This errors if more than one token is used as an output with this policy id
      _newOutput :: DynamicConfig
      !_newOutput = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
        [TxOut {txOutDatum}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Impossible. No minted output."

      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleToken
          txInfoMint
          thisCurrencySymbol
          ncTokenName
     in
      traceIfFalse "Missing significant UTxO!" hasUTxO
        && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted
mkNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: NftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkNftMinter config a (unsafeFromBuiltinData b))

policy :: NftConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

plutusScript :: NftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: NftConfig -> Validator
validator = Validator . plutusScript

nftMinterPolicyId :: NftConfig -> CurrencySymbol
nftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: NftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

nftMinter :: NftConfig -> PlutusScript PlutusScriptV2
nftMinter =
  PlutusScriptSerialised
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
  { cAddressCredential :: Credential
  , cAddressStakingCredential :: BuiltinData
  }

data ConfigurationTxOut = ConfigurationTxOut
  { cTxOutAddress :: ConfigurationAddress
  , cTxOutValue :: Value
  , cTxOutDatum :: OutputDatum
  , cTxOutReferenceScript :: BuiltinData
  }

data ConfigurationTxInInfo = ConfigurationTxInInfo
  { cTxInInfoOutRef :: TxOutRef
  , cTxInInfoResolved :: ConfigurationTxOut
  }

newtype ConfigurationScriptPurpose = ConfigurationSpend TxOutRef

data ConfigurationScriptContext = ConfigurationScriptContext
  { cScriptContextTxInfo :: ConfigurationTxInfo
  , cScriptContextPurpose :: ConfigurationScriptPurpose
  }

data ConfigurationTxInfo = ConfigurationTxInfo
  { cTxInfoInputs :: [ConfigurationTxInInfo]
  , cTxInfoReferenceInputs :: [ConfigurationTxInInfo]
  , cTxInfoOutputs :: [ConfigurationTxOut]
  , cTxInfoFee :: BuiltinData
  , cTxInfoMint :: Value
  , cTxInfoDCert :: BuiltinData
  , cTxInfoWdrl :: BuiltinData
  , cTxInfoValidRange :: POSIXTimeRange
  , cTxInfoSignatories :: [PubKeyHash]
  , cTxInfoRedeemers :: BuiltinData
  , cTxInfoData :: Map DatumHash Datum
  , cTxInfoId :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

data ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''ConfigurationAddress
unstableMakeIsData ''ConfigurationTxOut
unstableMakeIsData ''ConfigurationTxInInfo
makeIsDataIndexed ''ConfigurationScriptPurpose [('ConfigurationSpend, 1)]
unstableMakeIsData ''ConfigurationScriptContext
unstableMakeIsData ''ConfigurationTxInfo
makeLift ''ConfigurationValidatorConfig

ownValue :: [ConfigurationTxInInfo] -> TxOutRef -> Value
ownValue ins txOutRef = go ins
  where
    go = \case
      [] -> traceError "The impossible happened"
      ConfigurationTxInInfo {cTxInInfoOutRef, cTxInInfoResolved = ConfigurationTxOut {cTxOutValue}} : xs ->
        if cTxInInfoOutRef == txOutRef
          then cTxOutValue
          else go xs

validateConfiguration ::
  ConfigurationValidatorConfig ->
  DynamicConfig ->
  BuiltinData ->
  ConfigurationScriptContext ->
  Bool
validateConfiguration
  ConfigurationValidatorConfig {..}
  DynamicConfig {..}
  _
  ConfigurationScriptContext
    { cScriptContextTxInfo = ConfigurationTxInfo {..}
    , cScriptContextPurpose = ConfigurationSpend thisOutRef
    } =
    let
      thisScriptValue :: Value
      !thisScriptValue = ownValue cTxInfoInputs thisOutRef

      hasConfigurationNft :: Bool
      !hasConfigurationNft = hasOneOfToken cvcConfigNftCurrencySymbol cvcConfigNftTokenName thisScriptValue

      hasTallyNft :: Value -> Bool
      hasTallyNft = hasSymbolInValue dcTallyNft

      TallyState {tsProposal = proposal, ..} = case filter (hasTallyNft . cTxOutValue . cTxInInfoResolved) cTxInfoReferenceInputs of
        [] -> traceError "Missing tally NFT"
        [ConfigurationTxInInfo {cTxInInfoResolved = ConfigurationTxOut {..}}] -> convertDatum cTxInfoData cTxOutDatum
        _ -> traceError "Too many NFT values"

      upgradeMinter :: CurrencySymbol
      upgradeMinter = case proposal of
        Upgrade u -> u
        _ -> traceError "Not an upgrade proposal"

      totalVotes :: Integer
      !totalVotes = tsFor + tsAgainst

      relativeMajority :: Integer
      !relativeMajority = (totalVotes * 1000) `divide` dcTotalVotes

      majorityPercent :: Integer
      !majorityPercent = (tsFor * 1000) `divide` totalVotes

      hasEnoughVotes :: Bool
      !hasEnoughVotes =
        traceIfFalse "relative majority is too low" (relativeMajority >= dcUpgradRelativeMajorityPercent)
          && traceIfFalse "majority is too small" (majorityPercent >= dcUpgradeMajorityPercent)

      -- Make sure the upgrade token was minted
      hasUpgradeMinterToken :: Bool
      !hasUpgradeMinterToken = hasTokenInValue upgradeMinter "validateConfiguration, upgradeMinter" cTxInfoMint

      isAfterTallyEndTime :: Bool
      isAfterTallyEndTime = (tsProposalEndTime + POSIXTime dcProposalTallyEndOffset) `before` cTxInfoValidRange
     in
      traceIfFalse "Missing configuration nft" hasConfigurationNft
        && traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
        && traceIfFalse "Not minting upgrade token" hasUpgradeMinterToken
        && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime

wrapValidateConfiguration ::
  ConfigurationValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidateConfiguration cfg x y z =
  check
    ( validateConfiguration
        cfg
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

configurationValidator :: ConfigurationValidatorConfig -> Validator
configurationValidator cfg =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        }
   in
    Plutonomy.optimizeUPLCWith optimizerSettings $
      Plutonomy.validatorToPlutus $
        Plutonomy.mkValidatorScript $
          $$(PlutusTx.compile [||wrapValidateConfiguration||])
            `applyCode` liftCode cfg

configurationValidatorHash :: ConfigurationValidatorConfig -> ValidatorHash
configurationValidatorHash = validatorHash . configurationValidator

configurationScript :: ConfigurationValidatorConfig -> PlutusScript PlutusScriptV2
configurationScript =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . configurationValidator
