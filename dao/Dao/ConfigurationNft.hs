{- |
Module: Dao.ConfigurationNft
Description: Contains all the configuration specific types.
-}
module Dao.ConfigurationNft (
  -- * Script argument,containing relevant CurrenySymbol and TokenName
  ConfigurationValidatorConfig (..),
  -- | Script argument,containing relevant TxOutRef and TokenName for minting policy
  NftConfig (..),
) where

import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (makeLift)

-- | Used as an argument to the `Dao.ConfigurationNft.Script.mkConfigurationNftPolicy` minting policy script
data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  -- ^ The UTXO to be spent in the transaction
  , ncTokenName :: TokenName
  -- ^ The expected token name of the newly minted config NFT
  }

makeLift ''NftConfig

{- | Used as an argument to the validator scripts that need access
 to the `Dao.Types.DynamicConfigDatum` config in their reference inputs
-}
data ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  -- ^ The symbol of the NFT that marks the UTXO that contains
  -- the `Dao.Types.DynamicConfigDatum` config
  , cvcConfigNftTokenName :: TokenName
  -- ^ The token name of the NFT that marks the UTXO that contains
  -- the `Dao.Types.DynamicConfigDatum` config
  }

makeLift ''ConfigurationValidatorConfig
