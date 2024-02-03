{- |
Module      : Spec.Tally.Script
Description : Tally scripts
-}
module Spec.Tally.Script (
  -- * Minting policy
  tallyConfigNftTypedMintingPolicy,
  tallyConfigNftValue,
  tallyConfigNftCurrencySymbol,

  -- * Validator
  tallyNftTypedValidator,
  tallyValidatorScriptHash,
)
where

import Dao.ScriptArgument (ConfigurationValidatorConfig, TallyNftConfig (TallyNftConfig))
import Dao.Shared (mkUntypedValidator)
import Dao.Tally.Script (mkTallyNftMinter, validateTally)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  mkTypedValidator,
  scriptCurrencySymbol,
  scriptHash,
  toBuiltinPolicy,
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData, ($), (.))
import Spec.Configuration.SampleData (sampleConfigValidatorConfig)

-- Policy script and info
tallyConfigNftTypedMintingPolicy :: TallyNftConfig -> TypedPolicy ()
tallyConfigNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkTallyNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

tallyConfigNftCurrencySymbol :: TallyNftConfig -> CurrencySymbol
tallyConfigNftCurrencySymbol = scriptCurrencySymbol . tallyConfigNftTypedMintingPolicy

tallyConfigNftValue :: TallyNftConfig -> Value
tallyConfigNftValue nftCfg@(TallyNftConfig _ tokenName _ _) =
  singleton (tallyConfigNftCurrencySymbol nftCfg) tokenName 1

-- Validator script and info
type TallyValidatorScript = TypedValidator TallyStateDatum ()

tallyNftTypedValidator :: TallyValidatorScript
tallyNftTypedValidator = tallyTypedValidator' sampleConfigValidatorConfig

tallyValidatorScriptHash :: ScriptHash
tallyValidatorScriptHash = scriptHash tallyNftTypedValidator

tallyTypedValidator' :: ConfigurationValidatorConfig -> TallyValidatorScript
tallyTypedValidator' config =
  mkTypedValidator
    (compiledTallyValidator `PlutusTx.applyCode` PlutusTx.liftCode config)

compiledTallyValidator ::
  PlutusTx.CompiledCode (ConfigurationValidatorConfig -> (BuiltinData -> BuiltinData -> BuiltinData -> ()))
compiledTallyValidator =
  $$(PlutusTx.compile [||mkUntypedValidator . validateTally||])
