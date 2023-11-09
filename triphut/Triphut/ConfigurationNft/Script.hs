{- |
Module: Triphut.ConfigurationNft.Script
Description: Triphut configuration related scripts.
Includes:
  - Minting policy for Triphut configuration.
  - Validator for upgrading the configuration.
-}
module Triphut.ConfigurationNft.Script (
  mkConfigurationNftPolicy,
  configurationNftMintingPolicy,
  configurationNftCurrencySymbol,
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
  DynamicConfigDatum (
    DynamicConfigDatum,
    dcProposalTallyEndOffset,
    dcTallyNft,
    dcTotalVotes,
    dcUpgradeMajorityPercent,
    dcUpgradeRelativeMajorityPercent
  ),
  ProposalType (Upgrade),
  TallyState (TallyState, tsAgainst, tsFor, tsProposal, tsProposalEndTime),
 )

{- | Policy for minting configuration NFT.

   This policy performs the following checks:

    - The UTXO, referenced in the `ncInitialUtxo` field of
      the `NftConfig` argument, is spent in the transaction.
    - The token name matches the `ncTokenName` field of the `NftConfig` argument.
    - Exactly one config NFT is minted with the valid token name.
    - There is exactly one output containing the NFT.
    - This output contains a valid 'Triphut.Types.DynamicConfigDatum' datum.
-}
mkConfigurationNftPolicy :: NftConfig -> BuiltinData -> ScriptContext -> Bool
mkConfigurationNftPolicy
  NftConfig {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      -- Use with filter to find the config output containing the NFT
      hasWitness :: Value -> Bool
      hasWitness = hasTokenInValueNoErrors thisCurrencySymbol

      -- Ensure there is exactly one output that contains the configuration datum
      -- The `convertDatum` helper will throw an error if the output datum is not found
      _newOutput :: DynamicConfigDatum
      !_newOutput = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
        [TxOut {txOutDatum}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Impossible. No valid minted output."

      -- Ensure that the reference UTXO is spent
      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == ncInitialUtxo) txInfoInputs

      -- Ensure that only one valid token is minted
      -- The token name must match the `ncTokenName` from `NftConfig` argument
      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          ncTokenName
     in
      traceIfFalse "Referenced UTXO should be spent" hasUTxO
        && traceIfFalse "Exactly one valid token should be minted" onlyOneTokenMinted
mkConfigurationNftPolicy _ _ _ = traceError "Wrong type of script purpose!"

{- | The `configurationNftMintingPolicy` script built from `mkConfigurationNftPolicy`
 Takes an `NftConfig` as its argument
-}
configurationNftMintingPolicy :: NftConfig -> PlutusScript PlutusScriptV2
configurationNftMintingPolicy = policyToScript policy

{- | Currency symbol for the `configurationNftMintingPolicy` script
 Takes an `NftConfig` as its argument
-}
configurationNftCurrencySymbol :: NftConfig -> CurrencySymbol
configurationNftCurrencySymbol = mpsSymbol . mintingPolicyHash . policy

-- Build the policy
policy :: NftConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

wrappedPolicy :: NftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkConfigurationNftPolicy config a (unsafeFromBuiltinData b))

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
  DynamicConfigDatum ->
  BuiltinData ->
  ConfigurationScriptContext ->
  Bool
validateConfiguration
  ConfigurationValidatorConfig {..}
  DynamicConfigDatum {..}
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
