{- |
Module: Dao.Index
Description: Contains all the index specific types.
-}
module Dao.Index (
  -- * Datum
  IndexNftDatum (..),

  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  IndexNftConfig (..),
) where

import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value as V
import Plutus.V2.Ledger.Tx (TxOutRef)
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
  , incIndexValidator :: ValidatorHash
  -- ^ The hash of the `Dao.Index.Script.validateIndex` script,
  -- the policy uses this to ensure the newly minted token is sent to this index validator
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig
