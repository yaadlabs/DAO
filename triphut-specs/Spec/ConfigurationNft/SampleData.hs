{- |
Module      : Spec.ConfigurationNft.SampleData
Description : ConfigurationNft sample data for tests
-}
module Spec.ConfigurationNft.SampleData (
  sampleConfigValidatorConfig,
) where

import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)
import Triphut.ConfigurationNft (ConfigurationValidatorConfig (..))

sampleConfigValidatorConfig :: ConfigurationValidatorConfig
sampleConfigValidatorConfig =
  ConfigurationValidatorConfig
    { cvcConfigNftCurrencySymbol = dummyConfigNftSymbol
    , cvcConfigNftTokenName = dummyConfigNftTokenName
    }
