{- |
Module: Dao.Tally
Description: Contains all the tally specific types.
-}
module Dao.Tally (
  -- * Script arguments, containing relevant CurrencySymbol and TokenName
  TallyNftConfig (..),
) where

import PlutusLedgerApi.V1.Value (TokenName)
import PlutusLedgerApi.V2 (CurrencySymbol)
import PlutusTx (makeLift)

-- | Tally policy configuration
data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  }

makeLift ''TallyNftConfig
