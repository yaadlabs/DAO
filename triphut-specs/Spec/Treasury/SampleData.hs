{- |
Module      : Spec.Treasury.SampleData
Description : Treasury sample data for tests
-}
module Spec.Treasury.SampleData (
  sampleTreasuryValidatorConfig,
) where

import Spec.Values (dummyConfigNftSymbol, dummyConfigNftTokenName)
import Triphut.Treasury (
  TreasuryValidatorConfig (
    TreasuryValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
 )

-- | Sample tally config
sampleTreasuryValidatorConfig :: TreasuryValidatorConfig
sampleTreasuryValidatorConfig =
  TreasuryValidatorConfig
    { tvcConfigNftCurrencySymbol = dummyConfigNftSymbol
    , tvcConfigNftTokenName = dummyConfigNftTokenName
    }
