{- |
Module: Dao.Treasury
Description: Contains all the treasury specific types.
-}
module Dao.Treasury (
  -- Script context related type
  TreasuryValidatorConfig (..),
) where

import Plutus.V1.Ledger.Value as V
import PlutusTx (makeLift)

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

makeLift ''TreasuryValidatorConfig
