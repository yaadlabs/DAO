{- |
Module      : Spec.ConfigurationNft.SampleData
Description : ConfigurationNft sample data for tests
-}
module Spec.ConfigurationNft.SampleData (
  sampleConfigValidatorConfig,
) where

import Dao.ScriptArgument (ConfigurationValidatorConfig (..))
import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)

sampleConfigValidatorConfig :: ConfigurationValidatorConfig
sampleConfigValidatorConfig =
  ConfigurationValidatorConfig
    { cvcConfigNftCurrencySymbol = dummyConfigNftSymbol
    , cvcConfigNftTokenName = dummyConfigNftTokenName
    }
