module Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyConfigNftValue,
  dummyIndexConfigNftSymbol,
  dummyIndexConfigNftTokenName,
  dummyIndexConfigNftValue,
) where

import Plutus.V1.Ledger.Bytes (getLedgerBytes)
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), Value, singleton)
import PlutusTx.Prelude (($))

-- | Dummy Configuration
dummyConfigNftValue :: Value
dummyConfigNftValue = singleton dummyConfigNftSymbol dummyConfigNftTokenName 1

dummyConfigNftTokenName :: TokenName
dummyConfigNftTokenName = TokenName "config"

dummyConfigNftSymbol :: CurrencySymbol
dummyConfigNftSymbol =
  CurrencySymbol $
    getLedgerBytes "00000000000000000000000000000000000000000000000000000000"

-- | Dummy Index config
dummyIndexConfigNftValue :: Value
dummyIndexConfigNftValue = singleton dummyIndexConfigNftSymbol dummyIndexConfigNftTokenName 1

-- | Dummy Configuration NFT symbol
dummyIndexConfigNftSymbol :: CurrencySymbol
dummyIndexConfigNftSymbol =
  CurrencySymbol $
    getLedgerBytes "11111111111111111111111111111111111111111111111111111111"

dummyIndexConfigNftTokenName :: TokenName
dummyIndexConfigNftTokenName = TokenName "triphut_index"
