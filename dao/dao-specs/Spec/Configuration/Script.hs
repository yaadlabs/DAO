{- |
Module      : Spec.Configuration.Script
Description : Configuration scripts
-}
module Spec.Configuration.Script (
  -- * Validator
  upgradeConfigNftTypedValidator,

  -- * Minting policy
  configNftTypedMintingPolicy,
  configNftCurrencySymbol,
) where

import Dao.Configuration.Script (mkConfigurationNftPolicy, validateConfiguration)
import Dao.ScriptArgument (ConfigPolicyParams, ValidatorParams)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  mkTypedValidator,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData, ($), (.))
import Spec.Configuration.SampleData (sampleValidatorParams)
import Spec.SpecUtils (mkUntypedValidator)

-- Policy script and info
configNftTypedMintingPolicy :: ConfigPolicyParams -> TypedPolicy ()
configNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkConfigurationNftPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

configNftCurrencySymbol :: ConfigPolicyParams -> CurrencySymbol
configNftCurrencySymbol = scriptCurrencySymbol . configNftTypedMintingPolicy

-- Validator script and info
type ConfigUpgradeValidatorScript = TypedValidator DynamicConfigDatum ()

upgradeConfigNftTypedValidator :: ConfigUpgradeValidatorScript
upgradeConfigNftTypedValidator = upgradeConfigTypedValidator' sampleValidatorParams

upgradeConfigTypedValidator' :: ValidatorParams -> ConfigUpgradeValidatorScript
upgradeConfigTypedValidator' config =
  mkTypedValidator
    (compiledConfigValidator `PlutusTx.applyCode` PlutusTx.liftCode config)

compiledConfigValidator ::
  PlutusTx.CompiledCode (ValidatorParams -> (BuiltinData -> BuiltinData -> BuiltinData -> ()))
compiledConfigValidator =
  $$(PlutusTx.compile [||mkUntypedValidator . validateConfiguration||])
