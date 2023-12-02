{- |
Module: Dao.Tally
Description: Contains all the tally specific types.
-}
module Dao.Tally (
  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  TallyNftConfig (..),
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
