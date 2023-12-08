{- |
Module: Dao.Index
Description: Contains all the index specific types.
-}
module Dao.Index (
  -- * Datum
  IndexNftDatum (..),

  -- * Script argument, containing relevant UTXO, TokenName and hash of validator
  IndexNftConfig (..),
) where

import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Tx (TxOutRef)
import PlutusLedgerApi.V1.Value (TokenName)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.Prelude (Integer)

-- | Datum that is used to keep track of number of proposals
newtype IndexNftDatum = IndexNftDatum
  { indIndex :: Integer
  -- ^ Corresponds to the number of proposals
  -- `Dao.Index.Script.validateIndex` ensures that this value is
  -- incremented by one during a create proposal transaction
  }

-- | Used as an argument to the `Dao.Index.Script.mkIndexNftMinter` minting policy script
data IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TxOutRef
  -- ^ The UTXO to be spent in the transaction
  , incTokenName :: TokenName
  -- ^ The expected token name of the newly minted config NFT
  , incIndexValidator :: ScriptHash -- ValidatorHash

  -- ^ The hash of the `Dao.Index.Script.validateIndex` script,
  -- the policy uses this to ensure the newly minted token is sent to this index validator
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig
