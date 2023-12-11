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

import Dao.Configuration.Script (configurationValidatorCompiledCode, mkConfigurationNftPolicy)
import Dao.ScriptArgument (NftConfig)
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.Configuration.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')

-- Policy script and info
configNftTypedMintingPolicy :: NftConfig -> TypedPolicy ()
configNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkConfigurationNftPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

configNftCurrencySymbol :: NftConfig -> CurrencySymbol
configNftCurrencySymbol = scriptCurrencySymbol . configNftTypedMintingPolicy

-- Validator script and info
type ConfigUpgradeValidatorScript = TypedValidator DynamicConfigDatum ()

upgradeConfigNftTypedValidator :: ConfigUpgradeValidatorScript
upgradeConfigNftTypedValidator = mkTypedValidator' configurationValidatorCompiledCode sampleConfigValidatorConfig
