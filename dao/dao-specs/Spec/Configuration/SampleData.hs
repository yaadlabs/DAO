{- |
Module      : Spec.Configuration.SampleData
Description : Configuration sample data for tests
-}
module Spec.Configuration.SampleData (
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
