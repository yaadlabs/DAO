{- |
Module: Dao.Index.Script
Description: Dao index related scripts. It includes:
  - Index minting policy script.
  - Index validator script.
-}
module Dao.Index.Script (
  -- * Minting policy
  mkIndexMinter,
  indexPolicyCompiledCode,

  -- * Validator
  validateIndex,
  indexValidatorCompiledCode,
) where

import Dao.ScriptArgument (
  IndexPolicyParams (
    IndexPolicyParams,
    ipIndexValidator,
    ipInitialUtxo,
    ipTokenName
  ),
 )
import Dao.Shared (
  convertDatum,
  hasSingleTokenWithSymbolAndTokenName,
  hasTokenInValueNoErrors,
  untypedPolicy,
  wrapValidate',
 )
import LambdaBuffers.ApplicationTypes.Index (
  IndexDatum (
    IndexDatum,
    indexDatum'index
  ),
 )
import PlutusLedgerApi.V1.Address (Address (addressCredential))
import PlutusLedgerApi.V1.Credential (Credential (ScriptCredential))
import PlutusLedgerApi.V1.Value (
  Value,
  geq,
 )
import PlutusLedgerApi.V2 (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting, Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs
  ),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
 )
import PlutusLedgerApi.V2.Contexts (
  findTxInByTxOutRef,
  getContinuingOutputs,
 )
import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude (
  Bool,
  BuiltinData,
  Maybe (Just, Nothing),
  any,
  const,
  filter,
  mempty,
  traceError,
  traceIfFalse,
  (&&),
  (+),
  (==),
 )

{- | Validator for index.

   This validator performs the following checks:

    - The 'index' field of the 'LambdaBuffers.ApplicationTypes.Index.IndexDatum' is incremented when we
      create a new proposal (a new 'TallyStateDatum' is created and paid to the tally validator)
    - The index NFT stays at the validator
-}
validateIndex ::
  IndexDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateIndex
  IndexDatum {indexDatum'index = inputIndex}
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
      (!outputValue, !IndexDatum {indexDatum'index = outputIndex}) =
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
validateIndex _ _ _ = traceError "Wrong script purpose"

indexValidatorCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
indexValidatorCompiledCode =
  -- 'mempty' in this case is in place of the config argument to wrapValidate that we
  -- throw away here. And 'Value' is just an arbitrary legal type to satisfy the type checker
  -- (Same for 'indexScript' below)
  let wrapValidateIndex = wrapValidate' (const validateIndex) (mempty :: Value)
   in $$(PlutusTx.compile [||wrapValidateIndex||])

{- | Policy for minting index NFT.

   This policy performs the following checks:

    - The UTXO, referenced in the `ipInitialUtxo` field of
      the `IndexPolicyParams` argument, is spent in the transaction.
    - The token name matches the `ipTokenName` field of the `NftConfig` argument.
    - Exactly one valid config NFT is minted with the valid token name.
    - There is exactly one output containing the NFT.
    - This output contains a valid 'Dao.Index.IndexDatum' datum.
    - This 'index' field of this datum is set to zero.
    - The index output is at the index validator
      (Corresponding to the index script provided by the 'ipIndexValidator'
       field of the 'IndexPolicyParams' parameter)
-}
mkIndexMinter :: IndexPolicyParams -> BuiltinData -> ScriptContext -> Bool
mkIndexMinter
  IndexPolicyParams {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      -- Ensure that the reference UTXO is spent
      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == ipInitialUtxo) txInfoInputs

      -- Helper for filtering for index UTXO in the outputs
      hasWitness :: Value -> Bool
      hasWitness = hasTokenInValueNoErrors thisCurrencySymbol

      -- Get the index datum at the output marked by the index NFT
      (!IndexDatum {indexDatum'index}, !outputAddress) :: (IndexDatum, Address) =
        case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
          [TxOut {..}] -> (convertDatum txInfoData txOutDatum, txOutAddress)
          _ -> traceError "Should be exactly one valid minted output."

      -- Ensure that the initial index in the IndexDatum is set to zero
      initialIndexIsZero :: Bool
      !initialIndexIsZero = indexDatum'index == 0

      -- The NFT must be at the address of the index validator
      outputIsValidator :: Bool
      outputIsValidator = addressCredential outputAddress == ScriptCredential ipIndexValidator

      -- Ensure exactly one valid index token is minted
      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          ipTokenName
     in
      traceIfFalse "Reference UTXO should be spent" hasUTxO
        && traceIfFalse "Exactly one valid token should be minted" onlyOneTokenMinted
        && traceIfFalse "Initial index should be set to zero" initialIndexIsZero
        && traceIfFalse "Index NFT must be sent to the Index validator" outputIsValidator
mkIndexMinter _ _ _ = traceError "Wrong type of script purpose!"

untypedIndexPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedIndexPolicy = untypedPolicy mkIndexMinter

indexPolicyCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
indexPolicyCompiledCode = $$(PlutusTx.compile [||untypedIndexPolicy||])
