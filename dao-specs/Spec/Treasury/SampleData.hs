{- |
Module      : Spec.Treasury.SampleData
Description : Treasury sample data for tests
-}
module Spec.Treasury.SampleData (
  sampleTreasuryValidatorConfig,
) where

import Dao.Treasury (
  TreasuryValidatorConfig (
    TreasuryValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
 )
import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)

-- | Sample tally config
sampleTreasuryValidatorConfig :: TreasuryValidatorConfig
sampleTreasuryValidatorConfig =
  TreasuryValidatorConfig
    { tvcConfigNftCurrencySymbol = dummyConfigNftSymbol
    , tvcConfigNftTokenName = dummyConfigNftTokenName
    }
