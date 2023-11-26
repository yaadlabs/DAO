{- |
Module: Triphut.ConfigurationNft
Description: Contains the configuration specific script argument types.
-}
module Triphut.ConfigurationNft (
  -- * Script argument,containing relevant CurrenySymbol and TokenName
  ConfigurationValidatorConfig (..),

  -- * Script argument,containing relevant TxOutRef and TokenName

  -- for minting policy
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
