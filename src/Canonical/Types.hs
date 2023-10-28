module Canonical.Types (
  DynamicConfig (..),
  ProposalType (..),
  TallyState (..),
)
where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)
import PlutusTx (unstableMakeIsData)
import PlutusTx.Prelude (Bool (False), Integer, (&&), (==))
import PlutusTx.Prelude qualified as PlutusTx

{- | This represents the three possible types of proposals.
     A `Trip` proposal, a `General` proposal or an `Upgrade` proposal.
-}
data ProposalType
  = -- | Upgrade a proposal
    Upgrade
      CurrencySymbol
      -- ^ Symbol of the upgrade minting policy
  | -- | A general proposal
    General
      Address
      -- ^ General payment address
      Integer
      -- ^ General payment amount
  | -- | A trip proposal
    Trip
      Address
      -- ^ Travel agent address
      Address
      -- ^ Traveller address
      Integer
      -- ^ Total travel cost

instance PlutusTx.Eq ProposalType where
  Upgrade a == Upgrade b = a == b
  General a b == General c d = a == c && b == d
  Trip a b c == Trip d e f = a == d && b == e && c == f
  _ == _ = False

data TallyState = TallyState
  { tsProposal :: ProposalType
  , tsProposalEndTime :: POSIXTime
  , tsFor :: Integer
  , tsAgainst :: Integer
  }

instance PlutusTx.Eq TallyState where
  TallyState
    { tsProposal = xProposal
    , tsProposalEndTime = xProposalEndTime
    , tsFor = xFor
    , tsAgainst = xAgainst
    }
    == TallyState
      { tsProposal = yProposal
      , tsProposalEndTime = yProposalEndTime
      , tsFor = yFor
      , tsAgainst = yAgainst
      } =
      xProposal == yProposal
        && xProposalEndTime == yProposalEndTime
        && xFor == yFor
        && xAgainst == yAgainst

data DynamicConfig = DynamicConfig
  { dcTallyIndexNft :: CurrencySymbol
  , dcTallyNft :: CurrencySymbol
  , dcTallyValidator :: ValidatorHash
  , dcTreasuryValidator :: ValidatorHash
  , dcConfigurationValidator :: ValidatorHash
  , dcVoteCurrencySymbol :: CurrencySymbol
  , dcVoteTokenName :: TokenName
  , dcVoteValidator :: ValidatorHash
  , dcUpgradeMajorityPercent :: Integer -- times a 1000
  , dcUpgradRelativeMajorityPercent :: Integer -- times a 1000
  , dcGeneralMajorityPercent :: Integer -- times a 1000
  , dcGeneralRelativeMajorityPercent :: Integer -- times a 1000
  , dcTripMajorityPercent :: Integer -- times a 1000
  , dcTripRelativeMajorityPercent :: Integer -- times a 1000
  , dcTotalVotes :: Integer
  , dcVoteNft :: CurrencySymbol
  , dcVoteFungibleCurrencySymbol :: CurrencySymbol
  , dcVoteFungibleTokenName :: TokenName
  , dcProposalTallyEndOffset :: Integer -- in milliseconds
  , dcMaxGeneralDisbursement :: Integer
  , dcMaxTripDisbursement :: Integer
  , dcAgentDisbursementPercent :: Integer -- times a 1000
  , dcFungibleVotePercent :: Integer -- times a 1000
  }

unstableMakeIsData ''DynamicConfig
unstableMakeIsData ''TallyState
unstableMakeIsData ''ProposalType
