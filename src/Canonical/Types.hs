module Canonical.Types where
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts
import           PlutusTx.Prelude
import           PlutusTx

data TallyState = TallyState
  { tsProposal :: TxOutRef
  , tsFor      :: Integer
  , tsAgainst  :: Integer
  }

data ProposalType
  = Upgrade
      { ptUpgradeMinter :: CurrencySymbol
      }

data Proposal = Proposal
  { pEndTime :: POSIXTime
  , pType    :: ProposalType
  }

data DynamicConfig = DynamicConfig
  { dcTallyIndexNft                 :: CurrencySymbol
  , dcTallyNft                      :: CurrencySymbol
  , dcTallyTokenName                :: TokenName
  , dcTallyValidator                :: ValidatorHash
  , dcTreasuryValidator             :: ValidatorHash
  , dcConfigurationValidator        :: ValidatorHash
  , dcVoteCurrencySymbol            :: CurrencySymbol
  , dcVoteTokenName                 :: TokenName
  , dcVoteValidator                 :: ValidatorHash
  , dcUpgradeMajorityPercent        :: Integer -- times a 1000
  , dcUpgradRelativeMajorityPercent :: Integer -- times a 1000
  , dcTotalVotes                    :: Integer
  , dcVoteNft                       :: CurrencySymbol
  , dcVoteFungibleCurrencySymbol    :: CurrencySymbol
  , dcVoteFungibleTokenName         :: TokenName
  , dcProposalTallyEndOffset        :: Integer -- in milliseconds
  }

unstableMakeIsData ''TallyState
unstableMakeIsData ''ProposalType
unstableMakeIsData ''Proposal
unstableMakeIsData ''DynamicConfig
