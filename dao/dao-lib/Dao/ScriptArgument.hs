{- |
Module: Dao.ScriptArgument
Description: Contains the types that are passed to scripts as an argument
-}
module Dao.ScriptArgument (
  -- * Validator script arguments
  ValidatorParams (..),

  -- * Minting policy script arguments
  ConfigPolicyParams (..),
  IndexPolicyParams (..),
  TallyPolicyParams (..),
) where

import PlutusLedgerApi.V1 (CurrencySymbol)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Tx (TxOutRef)
import PlutusLedgerApi.V1.Value (TokenName)
import PlutusTx (makeLift, unstableMakeIsData)

-- | Used as an argument to the `Dao.ConfigurationNft.Script.mkConfigurationNftPolicy` minting policy script
data ConfigPolicyParams = ConfigPolicyParams
  { cpInitialUtxo :: TxOutRef
  -- ^ The UTXO to be spent in the transaction
  , cpTokenName :: TokenName
  -- ^ The expected token name of the newly minted config NFT
  }

{- | Used as an argument to the validator scripts that need access
 to the `Dao.Types.DynamicConfigDatum` config in their reference inputs
-}
data ValidatorParams = ValidatorParams
  { vpConfigSymbol :: CurrencySymbol
  -- ^ The symbol of the NFT that marks the UTXO that contains
  -- the `Dao.Types.DynamicConfigDatum` config
  , vpConfigTokenName :: TokenName
  -- ^ The token name of the NFT that marks the UTXO that contains
  -- the `Dao.Types.DynamicConfigDatum` config
  }

unstableMakeIsData ''ConfigPolicyParams
unstableMakeIsData ''ValidatorParams

makeLift ''ConfigPolicyParams
makeLift ''ValidatorParams

-- | Used as an argument to the `Dao.Index.Script.mkIndexNftMinter` minting policy script
data IndexPolicyParams = IndexPolicyParams
  { ipInitialUtxo :: TxOutRef
  -- ^ The UTXO to be spent in the transaction
  , ipTokenName :: TokenName
  -- ^ The expected token name of the newly minted config NFT
  , ipIndexValidator :: ScriptHash
  -- ^ The hash of the `Dao.Index.Script.validateIndex` script,
  -- the policy uses this to ensure the newly minted token is sent to this index validator
  }

makeLift ''IndexPolicyParams
unstableMakeIsData ''IndexPolicyParams

-- | Tally policy configuration
data TallyPolicyParams = TallyPolicyParams
  { tpIndexSymbol :: CurrencySymbol
  , tpIndexTokenName :: TokenName
  , tpConfigSymbol :: CurrencySymbol
  , tpConfigTokenName :: TokenName
  }

makeLift ''TallyPolicyParams
unstableMakeIsData ''TallyPolicyParams
