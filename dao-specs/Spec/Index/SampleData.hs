{- |
Module      : Spec.Index.SampleData
Description : Index sample data for tests
-}
module Spec.Index.SampleData (
  validSampleIndexNftDatum,
  sampleIndexValidatorConfig,
) where

import Dao.Index (
  IndexNftDatum (IndexNftDatum, indIndex),
  IndexValidatorConfig (
    IndexValidatorConfig,
    ivcConfigNftCurrencySymbol,
    ivcConfigNftTokenName
  ),
 )
import Plutus.V1.Ledger.Value (adaSymbol, adaToken)

-- | Valid index datum with the initial index set to zero
validSampleIndexNftDatum :: IndexNftDatum
validSampleIndexNftDatum = IndexNftDatum {indIndex = 0}

-- | Sample index config
sampleIndexValidatorConfig :: IndexValidatorConfig
sampleIndexValidatorConfig =
  IndexValidatorConfig
    { ivcConfigNftCurrencySymbol = adaSymbol
    , ivcConfigNftTokenName = adaToken
    }
