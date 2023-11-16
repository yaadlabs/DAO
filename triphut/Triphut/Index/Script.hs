{- |
Module: Triphut.Index.Script
Description: Triphut index related scripts. It includes:
  - Index minting policy script.
  - Index validator script.
-}
module Triphut.Index.Script (
  -- * Minting policy
  mkIndexNftMinter,
  tallyIndexNftMinter,
  tallyIndexNftMinterPolicyId,

  -- * Validator
  indexScript,
  indexValidator,
  indexValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript, PlutusScriptV2)
import Plutus.V1.Ledger.Address (Address (addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  Validator,
  ValidatorHash,
  mkMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  Value,
  geq,
  mpsSymbol,
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting, Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoReferenceInputs
  ),
  findTxInByTxOutRef,
  getContinuingOutputs,
 )
import Plutus.V2.Ledger.Tx (
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
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
  Maybe (Just, Nothing),
  any,
  check,
  filter,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (+),
  (.),
  (==),
 )
import Triphut.Index (
  IndexNftConfig (
    IndexNftConfig,
    incIndexValidator,
    incInitialUtxo,
    incTokenName
  ),
  IndexNftDatum (IndexNftDatum, indIndex),
  IndexValidatorConfig (IndexValidatorConfig),
 )
import Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasSingleTokenWithSymbolAndTokenName,
  hasTokenInValueNoErrors,
  mintingPolicyHash,
  mkValidatorWithSettings,
  policyToScript,
  validatorHash,
  validatorToScript,
  wrapValidate,
 )

{- | Validator for index.

   This validator performs the following checks:

    - The 'index' field of the 'Triphut.Index.IndexNftDatum' is incremented when we
      create a new proposal (a new 'TallyStateDatum' is created and paid to the tally validator)
    - The index NFT stays at the validator
-}
validateIndex ::
  IndexValidatorConfig ->
  IndexNftDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateIndex
  IndexValidatorConfig {..}
  IndexNftDatum {indIndex = inputIndex}
  _
  ctx@ScriptContext
    { scriptContextTxInfo = info@TxInfo {..}
    , scriptContextPurpose = Spending thisOutRef
    } =
    let
      scriptValue :: Value
      !scriptValue = case findTxInByTxOutRef thisOutRef info of
        Nothing -> traceError "Impossible not input"
        Just TxInInfo {txInInfoResolved = TxOut {..}} -> txOutValue

      outputValue :: Value
      (!outputValue, !IndexNftDatum {indIndex = outputIndex}) =
        case getContinuingOutputs ctx of
          [TxOut {..}] -> (txOutValue, convertDatum txInfoData txOutDatum)
          _ -> traceError "wrong number of continuing outputs"

      outputValueGreaterThanInputValue :: Bool
      outputValueGreaterThanInputValue = outputValue `geq` scriptValue

      outputDatumIsIncremented :: Bool
      outputDatumIsIncremented = outputIndex == inputIndex + 1
     in
      traceIfFalse "output datum is not incremented" outputDatumIsIncremented
        && traceIfFalse "script value is not returned" outputValueGreaterThanInputValue
validateIndex _ _ _ _ = traceError "Wrong script purpose"

indexValidator :: IndexValidatorConfig -> Validator
indexValidator config = mkValidatorWithSettings compiledCode True
  where
    wrapValidateIndex = wrapValidate validateIndex
    compiledCode = $$(PlutusTx.compile [||wrapValidateIndex||]) `applyCode` liftCode config

indexValidatorHash :: IndexValidatorConfig -> ValidatorHash
indexValidatorHash = validatorHash . indexValidator

indexScript :: IndexValidatorConfig -> PlutusScript PlutusScriptV2
indexScript = validatorToScript indexValidator

{- | Policy for minting index NFT.

   This policy performs the following checks:

    - The UTXO, referenced in the `incInitialUtxo` field of
      the `IndexNftConfig` argument, is spent in the transaction.
    - The token name matches the `incTokenName` field of the `NftConfig` argument.
    - Exactly one valid config NFT is minted with the valid token name.
    - There is exactly one output containing the NFT.
    - This output contains a valid 'Triphut.Index.IndexNftDatum' datum.
    - This 'index' field of this datum is set to zero.
    - The index output is at the index validator
      (Corresponding to the index script provided by the 'incIndexValidator'
       field of the 'IndexNftConfig' parameter)
-}
mkIndexNftMinter :: IndexNftConfig -> BuiltinData -> ScriptContext -> Bool
mkIndexNftMinter
  IndexNftConfig {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      -- Ensure that the reference UTXO is spent
      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == incInitialUtxo) txInfoInputs

      -- Helper for filtering for index UTXO in the outputs
      hasWitness :: Value -> Bool
      hasWitness = hasTokenInValueNoErrors thisCurrencySymbol

      -- Get the index datum at the output marked by the index NFT
      (!IndexNftDatum {indIndex}, !outputAddress) :: (IndexNftDatum, Address) =
        case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
          [TxOut {..}] -> (convertDatum txInfoData txOutDatum, txOutAddress)
          _ -> traceError "Should be exactly one valid minted output."

      -- Ensure that the initial index in the IndexNftDatum is set to zero
      initialIndexIsZero :: Bool
      !initialIndexIsZero = indIndex == 0

      -- The NFT must be at the address of the index validator
      outputIsValidator :: Bool
      outputIsValidator = addressCredential outputAddress == ScriptCredential incIndexValidator

      -- Ensure exactly one valid index token is minted
      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          incTokenName
     in
      traceIfFalse "Reference UTXO should be spent" hasUTxO
        && traceIfFalse "Exactly one valid token should be minted" onlyOneTokenMinted
        && traceIfFalse "Initial index should be set to zero" initialIndexIsZero
        && traceIfFalse "Index NFT must be sent to the Index validator" outputIsValidator
mkIndexNftMinter _ _ _ = traceError "Wrong type of script purpose!"

wrappedPolicy :: IndexNftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkIndexNftMinter config a (unsafeFromBuiltinData b))

policy :: IndexNftConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

tallyIndexNftMinterPolicyId :: IndexNftConfig -> CurrencySymbol
tallyIndexNftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

tallyIndexNftMinter :: IndexNftConfig -> PlutusScript PlutusScriptV2
tallyIndexNftMinter = policyToScript policy
