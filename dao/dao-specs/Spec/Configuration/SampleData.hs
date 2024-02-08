{- |
Module      : Spec.Configuration.SampleData
Description : Configuration sample data for tests
-}
module Spec.Configuration.SampleData (
  sampleValidatorParams,
) where

import Dao.ScriptArgument (ValidatorParams (..))
import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)

sampleValidatorParams :: ValidatorParams
sampleValidatorParams =
  ValidatorParams
    { vpConfigSymbol = dummyConfigNftSymbol
    , vpConfigTokenName = dummyConfigNftTokenName
    }
