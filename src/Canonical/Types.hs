module Canonical.Types where
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts
import           PlutusTx.Prelude
import           PlutusTx

data ProposalType
  = Upgrade
      { ptUpgradeMinter :: CurrencySymbol
      }
  | General
      { ptGeneralPaymentAddress :: Address
      , ptGeneralPaymentValue   :: Integer
      }
  | Trip
      { ptTravelAgentAddress :: Address
      , ptTravelerAddress    :: Address
      , ptTotalTravelCost    :: Integer
      }

instance Eq ProposalType where
  x == y = case (x, y) of
    (Upgrade a, Upgrade b) -> a == b
    (General a b, General c d) -> a == c && b == d
    (Trip a b c, Trip d e f) -> a == d && b == e && c == f
    _                      -> False

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
  , dcGeneralMajorityPercent        :: Integer -- times a 1000
  , dcGeneralRelativeMajorityPercent:: Integer -- times a 1000
  , dcTripMajorityPercent           :: Integer -- times a 1000
  , dcTripRelativeMajorityPercent   :: Integer -- times a 1000
  , dcTotalVotes                    :: Integer
  , dcVoteNft                       :: CurrencySymbol
  , dcVoteFungibleCurrencySymbol    :: CurrencySymbol
  , dcVoteFungibleTokenName         :: TokenName
  , dcProposalTallyEndOffset        :: Integer -- in milliseconds
  , dcMaxGeneralDisbursement        :: Integer
  , dcMaxTripDisbursement           :: Integer
  , dcAgentDisbursementPercent      :: Integer -- times a 1000
  }

unstableMakeIsData ''TallyState
unstableMakeIsData ''ProposalType
unstableMakeIsData ''DynamicConfig
