module Canonical.Types where
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts
import           PlutusTx.Prelude
import           PlutusTx

data ProposalType
  = Upgrade
      { ptUpgradeMinter :: CurrencySymbol
      }

instance Eq ProposalType where
  x == y = case (x, y) of
    (Upgrade a, Upgrade b) -> a == b

data TallyState = TallyState
  { tsProposal        :: ProposalType
  , tsProposalEndTime :: POSIXTime
  , tsFor             :: Integer
  , tsAgainst         :: Integer
  }

instance Eq TallyState where
  TallyState
    { tsProposal        = xProposal
    , tsProposalEndTime = xProposalEndTime
    , tsFor             = xFor
    , tsAgainst         = xAgainst
    } ==
      TallyState
        { tsProposal        = yProposal
        , tsProposalEndTime = yProposalEndTime
        , tsFor             = yFor
        , tsAgainst         = yAgainst
        } =  xProposal        == yProposal
          && xProposalEndTime == yProposalEndTime
          && xFor             == yFor
          && xAgainst         == yAgainst



data DynamicConfig = DynamicConfig
  { dcTallyIndexNft                 :: CurrencySymbol
  , dcTallyNft                      :: CurrencySymbol
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
unstableMakeIsData ''DynamicConfig
