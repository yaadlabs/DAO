module Triphut.Index.Script (
  indexScript,
  indexValidatorHash,
  mkIndexNftMinter,
  tallyIndexNftMinter,
  tallyIndexNftMinterPolicyId,
) where

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Address (Address (addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  Script,
  Validator (Validator),
  ValidatorHash,
  mkMintingPolicyScript,
  unMintingPolicyScript,
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
  Bool (False),
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
  IndexValidatorConfig (IndexValidatorConfig, ivcNonce),
 )
import Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasSingleToken,
  hasTokenInValue,
  mintingPolicyHash,
  validatorHash,
  wrapValidate,
 )

-- | Nft Index Validator
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
        && ivcNonce == ivcNonce -- to help with testing
validateIndex _ _ _ _ = traceError "Wrong script purpose"

wrapValidateIndex :: IndexValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidateIndex = wrapValidate validateIndex

indexValidator :: IndexValidatorConfig -> Validator
indexValidator cfg =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        }
   in
    Plutonomy.optimizeUPLCWith optimizerSettings $
      Plutonomy.validatorToPlutus $
        Plutonomy.mkValidatorScript $
          $$(PlutusTx.compile [||wrapValidateIndex||])
            `applyCode` liftCode cfg

indexValidatorHash :: IndexValidatorConfig -> ValidatorHash
indexValidatorHash = validatorHash . indexValidator

indexScript :: IndexValidatorConfig -> PlutusScript PlutusScriptV2
indexScript =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . indexValidator

-- | Nft Index Policy
mkIndexNftMinter :: IndexNftConfig -> BuiltinData -> ScriptContext -> Bool
mkIndexNftMinter
  IndexNftConfig {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      hasWitness :: Value -> Bool
      hasWitness = hasTokenInValue thisCurrencySymbol "IndexNft Minter, hasWitness"

      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == incInitialUtxo) txInfoInputs

      (!IndexNftDatum {..}, !outputAddress) =
        case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
          [TxOut {..}] -> (convertDatum txInfoData txOutDatum, txOutAddress)
          _ -> traceError "Impossible. No minted output."

      initialIndexIsZero :: Bool
      !initialIndexIsZero = indIndex == 0

      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleToken
          txInfoMint
          thisCurrencySymbol
          incTokenName

      outputIsValidator :: Bool
      outputIsValidator = addressCredential outputAddress == ScriptCredential incIndexValidator
     in
      traceIfFalse "Missing significant UTxO!" hasUTxO
        && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted
        && traceIfFalse "Initial Index is not zero" initialIndexIsZero
        && traceIfFalse "Output is not index validator" outputIsValidator
mkIndexNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: IndexNftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkIndexNftMinter config a (unsafeFromBuiltinData b))

policy :: IndexNftConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

plutusScript :: IndexNftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: IndexNftConfig -> Validator
validator = Validator . plutusScript

tallyIndexNftMinterPolicyId :: IndexNftConfig -> CurrencySymbol
tallyIndexNftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: IndexNftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

tallyIndexNftMinter :: IndexNftConfig -> PlutusScript PlutusScriptV2
tallyIndexNftMinter =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . scriptAsCbor
