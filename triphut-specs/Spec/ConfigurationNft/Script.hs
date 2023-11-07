{- |
Module      : Spec.ConfigurationNft.Script
Description : ConfigurationNft scripts
-}
module Spec.ConfigurationNft.Script (
  configNftTypedValidator,
  configNftTypedMintingPolicy,
  configNftCurrencySymbol,
) where

import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator (TypedValidator),
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
  toV2,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Triphut.ConfigurationNft (ConfigurationValidatorConfig, NftConfig (..))
import Triphut.ConfigurationNft.Script (configurationValidator, mkNftMinter)
import Triphut.Types (DynamicConfig)

-- Policy script and info
configNftTypedMintingPolicy :: NftConfig -> TypedPolicy ()
configNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

configNftCurrencySymbol :: NftConfig -> CurrencySymbol
configNftCurrencySymbol = scriptCurrencySymbol . configNftTypedMintingPolicy

-- Validator script and info
type ConfigValidatorScript = TypedValidator DynamicConfig ()

configNftTypedValidator :: ConfigValidatorScript
configNftTypedValidator = mkTypedValidator' sampleConfigValidatorConfig

mkTypedValidator' :: ConfigurationValidatorConfig -> ConfigValidatorScript
mkTypedValidator' cfg = TypedValidator . toV2 $ configurationValidator cfg
