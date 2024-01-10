{- |
Module: Dao.ScriptArgument
Description: Contains the types that are passed to scripts as an argument
-}
module Dao.ScriptArgument (
  -- * Validator script arguments
  ConfigurationValidatorConfig (..),

  -- * Minting policy script arguments
  NftConfig (..),
  IndexNftConfig (..),
  TallyNftConfig (..),
) where

import PlutusLedgerApi.V1 (CurrencySymbol)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Tx (TxOutRef)
import PlutusLedgerApi.V1.Value (TokenName)
import PlutusTx (makeLift)

-- | Used as an argument to the `Dao.ConfigurationNft.Script.mkConfigurationNftPolicy` minting policy script
data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  -- ^ The UTXO to be spent in the transaction
  , ncTokenName :: TokenName
  -- ^ The expected token name of the newly minted config NFT
  }

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

makeLift ''NftConfig
makeLift ''ConfigurationValidatorConfig

-- | Used as an argument to the `Dao.Index.Script.mkIndexNftMinter` minting policy script
data IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TxOutRef
  -- ^ The UTXO to be spent in the transaction
  , incTokenName :: TokenName
  -- ^ The expected token name of the newly minted config NFT
  , incIndexValidator :: ScriptHash
  -- ^ The hash of the `Dao.Index.Script.validateIndex` script,
  -- the policy uses this to ensure the newly minted token is sent to this index validator
  }

makeLift ''IndexNftConfig

-- | Tally policy configuration
data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  }

makeLift ''TallyNftConfig
