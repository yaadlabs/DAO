{- |
Module: Triphut.Treasury
Description: Contains all the treasury specific types.
-}
module Triphut.Treasury (
  TreasuryValidatorConfig (..),
  Treasury,
) where

import Plutus.V1.Ledger.Value as V
import PlutusTx (makeLift)
import PlutusTx.Prelude (BuiltinData)

type Treasury = BuiltinData

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

makeLift ''TreasuryValidatorConfig
