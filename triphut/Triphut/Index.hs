{- |
Module: Triphut.Index
Description: Contains all the index specific types.
-}
module Triphut.Index (
  -- * Datum
  IndexNftDatum (..),

  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  IndexValidatorConfig (..),
  IndexNftConfig (..),
) where

import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value as V
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.Prelude (Integer)

newtype IndexNftDatum = IndexNftDatum
  { indIndex :: Integer
  }

data IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TxOutRef
  , incTokenName :: TokenName
  , incIndexValidator :: ValidatorHash
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig

-------------------------------------------------------------------------------
-- Nft Index Validator
-------------------------------------------------------------------------------

data IndexValidatorConfig = IndexValidatorConfig
  { ivcConfigNftCurrencySymbol :: CurrencySymbol
  , ivcConfigNftTokenName :: TokenName
  }

makeLift ''IndexValidatorConfig
