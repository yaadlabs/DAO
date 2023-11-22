{- |
Module: Triphut.Treasury
Description: Contains all the treasury specific types.
-}
module Triphut.Treasury (
  TreasuryValidatorConfig (..),
) where

import Plutus.V1.Ledger.Value as V
import PlutusTx (makeLift)

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

makeLift ''TreasuryValidatorConfig
