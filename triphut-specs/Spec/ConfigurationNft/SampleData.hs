{- |
Module      : Spec.ConfigurationNft.SampleData
Description : ConfigurationNft sample data for tests
-}
module Spec.ConfigurationNft.SampleData (
  sampleConfigValidatorConfig,
) where

import Plutus.V1.Ledger.Value (adaSymbol, adaToken)
import Triphut.ConfigurationNft (ConfigurationValidatorConfig (..))

sampleConfigValidatorConfig :: ConfigurationValidatorConfig
sampleConfigValidatorConfig =
  ConfigurationValidatorConfig
    { cvcConfigNftCurrencySymbol = adaSymbol
    , cvcConfigNftTokenName = adaToken
    }
