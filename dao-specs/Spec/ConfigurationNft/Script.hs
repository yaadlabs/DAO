{- |
Module      : Spec.ConfigurationNft.Script
Description : ConfigurationNft scripts
-}
module Spec.ConfigurationNft.Script (
  upgradeConfigNftTypedValidator,
  configNftTypedMintingPolicy,
  configNftCurrencySymbol,
) where

import Dao.ConfigurationNft (NftConfig)
import Dao.ConfigurationNft.Script (configurationValidator, mkConfigurationNftPolicy)
import Dao.Types (DynamicConfigDatum)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
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
upgradeConfigNftTypedValidator = mkTypedValidator' sampleConfigValidatorConfig configurationValidator
