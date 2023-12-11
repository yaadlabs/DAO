module Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyConfigNftValue,
  dummyIndexConfigNftSymbol,
  dummyIndexConfigNftTokenName,
  dummyIndexConfigNftValue,
  dummyTallySymbol,
  dummyTallyTokenName,
  dummyTallyValue,
  dummyVoteSymbol,
  dummyVoteTokenName,
  dummyVoteValue,
  dummyVoteFungibleSymbol,
  dummyVoteFungibleToken,
  dummyVoteFungibleValue,
  dummyTreasurySymbol,
  dummyTreasuryTokenName,
  dummyTreasuryValue,
) where

import PlutusLedgerApi.V1.Bytes (getLedgerBytes)
import PlutusLedgerApi.V1.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), Value, singleton)
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

-- | Dummy vote fungible
dummyVoteFungibleValue :: Value
dummyVoteFungibleValue = singleton dummyVoteFungibleSymbol dummyVoteFungibleToken 1

dummyVoteFungibleToken :: TokenName
dummyVoteFungibleToken = TokenName "vote_fungible"

dummyVoteFungibleSymbol :: CurrencySymbol
dummyVoteFungibleSymbol =
  CurrencySymbol $
    getLedgerBytes "66666666666666666666666666666666666666666666666666666666"

-- | Dummy treasury
dummyTreasuryValue :: Value
dummyTreasuryValue = singleton dummyTreasurySymbol dummyTreasuryTokenName 1

dummyTreasuryTokenName :: TokenName
dummyTreasuryTokenName = TokenName "treasury"

dummyTreasurySymbol :: CurrencySymbol
dummyTreasurySymbol =
  CurrencySymbol $
    getLedgerBytes "77777777777777777777777777777777777777777777777777777777"
