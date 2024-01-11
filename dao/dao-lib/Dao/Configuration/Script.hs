{- |
Module: Dao.Configuration.Script
Description: Dao configuration related scripts. It includes:
  - Minting policy for Dao configuration.
  - Validator for upgrading the configuration.
-}
module Dao.Configuration.Script (
  -- * Minting policy
  mkConfigurationNftPolicy,
  configPolicyUnappliedCompiledCode,
  configPolicyCompiledCode,
  testValidatorCompiled,
  alwaysMintsCompiled,

  -- * Validator
  validateConfiguration,
  configurationValidatorCompiledCode,
  configValidatorUnappliedCompiledCode,
) where

import Dao.ScriptArgument (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
  NftConfig (NftConfig, ncInitialUtxo, ncTokenName),
 )
import Dao.Shared (
  convertDatum,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  hasTokenInValue,
  hasTokenInValueNoErrors,
  mkUntypedPolicy,
  mkUntypedValidator,
  wrapValidate',
 )
import LambdaBuffers.ApplicationTypes.Configuration (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dynamicConfigDatum'proposalTallyEndOffset,
    dynamicConfigDatum'tallyNft,
    dynamicConfigDatum'totalVotes,
    dynamicConfigDatum'upgradeMajorityPercent,
    dynamicConfigDatum'upgradeRelativeMajorityPercent
  ),
 )
import LambdaBuffers.ApplicationTypes.Proposal (ProposalType (ProposalType'Upgrade))
import LambdaBuffers.ApplicationTypes.Tally (
  TallyStateDatum (
    TallyStateDatum,
    tallyStateDatum'against,
    tallyStateDatum'for,
    tallyStateDatum'proposal,
    tallyStateDatum'proposalEndTime
  ),
 )
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime))
import PlutusLedgerApi.V1.Value (Value)
import PlutusLedgerApi.V2 (CurrencySymbol)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting, Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoReferenceInputs,
    txInfoValidRange
  ),
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (TxOut, txOutDatum, txOutValue),
  TxOutRef,
 )
import PlutusTx (
  CompiledCode,
  applyCode,
  compile,
  liftCode,
  unsafeFromBuiltinData,
 )
import PlutusTx.Prelude (
  Bool,
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

{- | Policy for minting configuration NFT.

   This policy performs the following checks:

    - The UTXO, referenced in the `ncInitialUtxo` field of
      the `NftConfig` argument, is spent in the transaction.
    - The token name matches the `ncTokenName` field of the `NftConfig` argument.
    - Exactly one config NFT is minted with the valid token name.
    - There is exactly one output containing the NFT.
    - This output contains a valid 'LambdaBuffers.ApplicationTypes.Configuration.DynamicConfigDatum' datum.
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
        _ -> traceError "Should be exactly one valid minted output."

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

configPolicyUnappliedCompiledCode ::
  CompiledCode (NftConfig -> BuiltinData -> BuiltinData -> ())
configPolicyUnappliedCompiledCode =
  $$(PlutusTx.compile [||mkUntypedPolicy . mkConfigurationNftPolicy||])

untypedConfigPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedConfigPolicy nftConfig r context =
  check $
    mkConfigurationNftPolicy (unsafeFromBuiltinData nftConfig) r (unsafeFromBuiltinData context)

configPolicyCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
configPolicyCompiledCode = $$(PlutusTx.compile [||untypedConfigPolicy||])

testValidator :: Integer -> BuiltinData -> BuiltinData -> BuiltinData -> ()
testValidator _ _ _ _ = ()

untypedTestValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTestValidator p = testValidator (unsafeFromBuiltinData p)

testValidatorCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
testValidatorCompiled = $$(PlutusTx.compile [||untypedTestValidator||])

alwaysMints :: BuiltinData -> BuiltinData -> ()
alwaysMints _ _ = ()

alwaysMintsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysMintsCompiled = $$(PlutusTx.compile [||alwaysMints||])

{- | Validator for proposal upgrades.

   This validator performs the following checks:

    - There is exactly one 'LambdaBuffers.ApplicationTypes.Tally.TallyStateDatum' in the reference inputs,
      marked by the tally NFT
      (Corresponding tally CurrencySymbol is contained in the 'tallyNft' field of the 'DynamicConfigDatum')
    - There is a configuration token in the inputs
    - The proposal is an upgrade proposal (LambdaBuffers.ApplicationTypes.Proposal.ProposalType.Upgrade)
    - That one 'Upgrade' token was minted in the transaction with the CurrencySymbol specified in
      the 'ProposalType.Upgrade'
    - That the upgrade proposal has enough votes
        We do this by checking that the number of votes recorded in the 'TallyStateDatum' via the
        'For' and 'Against' fields are greater than or equal to the required majorities specified in
        the 'upgradeRelativeMajorityPercent' and 'upgradeMajorityPercent' fields of the 'DynamicConfigDatum'.
    - That the time period for voting on the proposal has passed.
        We do this by checking the 'proposalEndTime' (specified in the 'TallyStateDatum') added to the
        'proposalTallyEndOffset' (specified ) against the validity range of the transaction, ensuring they
        sum to a time before the transaction's validity range.
-}
validateConfiguration ::
  ConfigurationValidatorConfig ->
  DynamicConfigDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateConfiguration
  ConfigurationValidatorConfig {..}
  DynamicConfigDatum {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Spending thisOutRef
    } =
    let
      thisScriptValue :: Value
      !thisScriptValue = ownValue txInfoInputs thisOutRef

      -- Ensure there is a config token in the inputs
      hasConfigurationNft :: Bool
      !hasConfigurationNft = hasOneOfToken cvcConfigNftCurrencySymbol cvcConfigNftTokenName thisScriptValue

      -- Helper for filtering for tally UTXO in the reference inputs
      hasTallyNft :: Value -> Bool
      hasTallyNft = hasSymbolInValue dynamicConfigDatum'tallyNft

      -- Ensure there is exactly one output that contains the 'TallyStateDatum' datum
      -- The `convertDatum` helper will throw an error if the output datum is not found
      TallyStateDatum {tallyStateDatum'proposal = proposal, ..} =
        case filter (hasTallyNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [] -> traceError "Should be exactly one tally NFT in the reference inputs. None found."
          [TxInInfo {txInInfoResolved = TxOut {..}}] ->
            convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one tally NFT in the reference inputs. More than one found."

      -- Ensure that the 'ProposalType' set in the 'tsProposal' field
      -- of the 'TallyStateDatum' is 'Upgrade', and retrieve the upgrade symbol
      upgradeMinter :: CurrencySymbol
      upgradeMinter = case proposal of
        ProposalType'Upgrade u -> u
        _ -> traceError "Not an upgrade proposal"

      -- The total votes, for and against, in the 'TallyStateDatum'
      totalVotes :: Integer
      !totalVotes = tallyStateDatum'for + tallyStateDatum'against

      -- Calculate the majorities
      relativeMajority :: Integer
      !relativeMajority = (totalVotes * 1000) `divide` dynamicConfigDatum'totalVotes

      majorityPercent :: Integer
      !majorityPercent = (tallyStateDatum'for * 1000) `divide` totalVotes

      -- Ensure the upgrade proposal has sufficient votes by checking
      -- the majorities against the required amounts specified in the 'DynamicConfigDatum'
      hasEnoughVotes :: Bool
      !hasEnoughVotes =
        traceIfFalse
          "relative majority is too low"
          (relativeMajority >= dynamicConfigDatum'upgradeRelativeMajorityPercent)
          && traceIfFalse
            "majority is too small"
            (majorityPercent >= dynamicConfigDatum'upgradeMajorityPercent)

      -- Make sure the upgrade token was minted
      hasUpgradeMinterToken :: Bool
      !hasUpgradeMinterToken = hasTokenInValue upgradeMinter "validateConfiguration, upgradeMinter" txInfoMint

      -- Ensure the proposal has finished
      isAfterTallyEndTime :: Bool
      isAfterTallyEndTime =
        (tallyStateDatum'proposalEndTime + POSIXTime dynamicConfigDatum'proposalTallyEndOffset)
          `before` txInfoValidRange
     in
      traceIfFalse "Should be exactly one configuration NFT in the inputs" hasConfigurationNft
        && traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
        && traceIfFalse "Should be exactly one upgrade token minted" hasUpgradeMinterToken
        && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime
validateConfiguration _ _ _ _ = traceError "Wrong script purpose"

configValidatorUnappliedCompiledCode ::
  CompiledCode (ConfigurationValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ())
configValidatorUnappliedCompiledCode =
  $$(PlutusTx.compile [||mkUntypedValidator . validateConfiguration||])

configurationValidatorCompiledCode ::
  ConfigurationValidatorConfig ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
configurationValidatorCompiledCode config =
  $$(PlutusTx.compile [||wrapValidateConfiguration||]) `applyCode` liftCode config

wrapValidateConfiguration :: ConfigurationValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidateConfiguration = wrapValidate' validateConfiguration

ownValue :: [TxInInfo] -> TxOutRef -> Value
ownValue ins txOutRef = go ins
  where
    go = \case
      [] -> traceError "The impossible happened"
      TxInInfo {txInInfoOutRef, txInInfoResolved = TxOut {txOutValue}} : xs ->
        if txInInfoOutRef == txOutRef
          then txOutValue
          else go xs
