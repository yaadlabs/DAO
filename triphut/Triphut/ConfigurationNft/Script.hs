module Triphut.ConfigurationNft.Script (
  mkNftMinter,
  nftMinter,
  nftMinterPolicyId,
  configurationScript,
  configurationValidator,
  configurationValidatorHash,
) where

import Triphut.ConfigurationNft (
  ConfigurationScriptContext (
    ConfigurationScriptContext,
    cScriptContextPurpose,
    cScriptContextTxInfo
  ),
  ConfigurationScriptPurpose (ConfigurationSpend),
  ConfigurationTxInInfo (
    ConfigurationTxInInfo,
    cTxInInfoOutRef,
    cTxInInfoResolved
  ),
  ConfigurationTxInfo (
    ConfigurationTxInfo,
    cTxInfoData,
    cTxInfoInputs,
    cTxInfoMint,
    cTxInfoReferenceInputs,
    cTxInfoValidRange
  ),
  ConfigurationTxOut (
    ConfigurationTxOut,
    cTxOutDatum,
    cTxOutValue
  ),
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
  NftConfig (NftConfig, ncInitialUtxo, ncTokenName),
 )

import Cardano.Api.Shelley (PlutusScript, PlutusScriptV2)
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  Validator,
  ValidatorHash,
  mkMintingPolicyScript,
 )
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
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
  TxOut (TxOut, txOutDatum, txOutValue),
  TxOutRef,
 )
import PlutusTx (
  applyCode,
  compile,
  liftCode,
  unsafeFromBuiltinData,
 )
import PlutusTx.Prelude (
  Bool (True),
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
import Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  hasTokenInValue,
  hasTokenInValueNoErrors,
  mintingPolicyHash,
  mkValidatorWithSettings,
  policyToScript,
  validatorHash,
  validatorToScript,
  wrapValidate,
 )
import Triphut.Types (
  DynamicConfig (
    DynamicConfig,
    dcProposalTallyEndOffset,
    dcTallyNft,
    dcTotalVotes,
    dcUpgradeMajorityPercent,
    dcUpgradeRelativeMajorityPercent
  ),
  ProposalType (Upgrade),
  TallyState (TallyState, tsAgainst, tsFor, tsProposal, tsProposalEndTime),
 )

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
      hasWitness = hasTokenInValueNoErrors thisCurrencySymbol

      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == ncInitialUtxo) txInfoInputs

      _newOutput :: DynamicConfig
      !_newOutput = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
        [TxOut {txOutDatum}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Impossible. No valid minted output."

      _onlyOneTokenMinted :: Bool
      !_onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          ncTokenName
     in
      traceIfFalse "Missing significant UTxO!" hasUTxO
mkNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: NftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkNftMinter config a (unsafeFromBuiltinData b))

policy :: NftConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

nftMinterPolicyId :: NftConfig -> CurrencySymbol
nftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

nftMinter :: NftConfig -> PlutusScript PlutusScriptV2
nftMinter = policyToScript policy

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

      TallyState {tsProposal = proposal, ..} =
        case filter (hasTallyNft . cTxOutValue . cTxInInfoResolved) cTxInfoReferenceInputs of
          [] -> traceError "Missing tally NFT"
          [ConfigurationTxInInfo {cTxInInfoResolved = ConfigurationTxOut {..}}] ->
            convertDatum cTxInfoData cTxOutDatum
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
        traceIfFalse "relative majority is too low" (relativeMajority >= dcUpgradeRelativeMajorityPercent)
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

configurationValidator :: ConfigurationValidatorConfig -> Validator
configurationValidator config = mkValidatorWithSettings compiledCode True
  where
    compiledCode = $$(PlutusTx.compile [||wrapValidateConfiguration||]) `applyCode` liftCode config

wrapValidateConfiguration :: ConfigurationValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidateConfiguration = wrapValidate validateConfiguration

configurationValidatorHash :: ConfigurationValidatorConfig -> ValidatorHash
configurationValidatorHash = validatorHash . configurationValidator

configurationScript :: ConfigurationValidatorConfig -> PlutusScript PlutusScriptV2
configurationScript = validatorToScript configurationValidator

ownValue :: [ConfigurationTxInInfo] -> TxOutRef -> Value
ownValue ins txOutRef = go ins
  where
    go = \case
      [] -> traceError "The impossible happened"
      ConfigurationTxInInfo {cTxInInfoOutRef, cTxInInfoResolved = ConfigurationTxOut {cTxOutValue}} : xs ->
        if cTxInInfoOutRef == txOutRef
          then cTxOutValue
          else go xs
