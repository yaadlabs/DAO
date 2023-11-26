{- |
Module: Triphut.Tally
Description: Contains all the tally specific types.
-}
module Triphut.Tally (
  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  TallyNftConfig (..),
  TallyValidatorConfig (..),
) where

import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)
import PlutusTx (makeLift)

-- | Tally policy configuration
data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  }

makeLift ''TallyNftConfig

data TallyValidatorConfig = TallyValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

makeLift ''TallyValidatorConfig
