module Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyConfigNftValue,
  dummyIndexConfigNftSymbol,
  dummyIndexConfigNftTokenName,
  dummyIndexConfigNftValue,
  dummyVoteConfigNftValue,
  dummyVoteConfigNftSymbol,
  dummyVoteConfigNftTokenName,
  dummyTallySymbol,
  dummyTallyTokenName,
  dummyTallyValue,
  dummyVoteSymbol,
  dummyVoteTokenName,
  dummyVoteValue,
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

dummyIndexConfigNftSymbol :: CurrencySymbol
dummyIndexConfigNftSymbol =
  CurrencySymbol $
    getLedgerBytes "11111111111111111111111111111111111111111111111111111111"

dummyIndexConfigNftTokenName :: TokenName
dummyIndexConfigNftTokenName = TokenName "triphut_index"

-- | Dummy vote configuration
dummyVoteConfigNftValue :: Value
dummyVoteConfigNftValue = singleton dummyVoteConfigNftSymbol dummyVoteConfigNftTokenName 1

dummyVoteConfigNftTokenName :: TokenName
dummyVoteConfigNftTokenName = TokenName "vote_config"

dummyVoteConfigNftSymbol :: CurrencySymbol
dummyVoteConfigNftSymbol =
  CurrencySymbol $
    getLedgerBytes "22222222222222222222222222222222222222222222222222222222"

-- | Dummy tally
dummyTallyValue :: Value
dummyTallyValue = singleton dummyTallySymbol dummyTallyTokenName 1

dummyTallyTokenName :: TokenName
dummyTallyTokenName = TokenName "0"

dummyTallySymbol :: CurrencySymbol
dummyTallySymbol =
  CurrencySymbol $
    getLedgerBytes "33333333333333333333333333333333333333333333333333333333"

-- | Dummy vote
dummyVoteValue :: Value
dummyVoteValue = singleton dummyVoteSymbol dummyVoteTokenName 1

dummyVoteTokenName :: TokenName
dummyVoteTokenName = TokenName "vote"

dummyVoteSymbol :: CurrencySymbol
dummyVoteSymbol =
  CurrencySymbol $
    getLedgerBytes "44444444444444444444444444444444444444444444444444444444"
