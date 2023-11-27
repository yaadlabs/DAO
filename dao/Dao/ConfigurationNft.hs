{- |
Module: Dao.ConfigurationNft
Description: Contains all the configuration specific types.
-}
module Dao.ConfigurationNft (
  -- * Script argument,containing relevant CurrenySymbol and TokenName
  ConfigurationValidatorConfig (..),

  -- * Script argument,containing relevant TxOutRef and TokenName
  NftConfig (..),
) where

import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (makeLift)

data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  , ncTokenName :: TokenName
  }

makeLift ''NftConfig

data ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName :: TokenName
  }

makeLift ''ConfigurationValidatorConfig
