{- |
Module: Triphut.Types
Description: Contains the `DynamicConfigDatum` type, the `TallyStateDatum` type, and the `ProposalType` type.
-}
module Triphut.Types (
  -- * Datums
  DynamicConfigDatum (..),
  TallyStateDatum (..),

  -- * Main proposal type
  ProposalType (..),
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

-- | Tally state datum
data TallyStateDatum = TallyStateDatum
  { tsProposal :: ProposalType
  , tsProposalEndTime :: POSIXTime
  , tsFor :: Integer
  , tsAgainst :: Integer
  }

instance PlutusTx.Eq TallyStateDatum where
  TallyStateDatum
    { tsProposal = xProposal
    , tsProposalEndTime = xProposalEndTime
    , tsFor = xFor
    , tsAgainst = xAgainst
    }
    == TallyStateDatum
      { tsProposal = yProposal
      , tsProposalEndTime = yProposalEndTime
      , tsFor = yFor
      , tsAgainst = yAgainst
      } =
      xProposal == yProposal
        && xProposalEndTime == yProposalEndTime
        && xFor == yFor
        && xAgainst == yAgainst

-- | DynamicConfig Datum holds the main info needed for the contracts.
data DynamicConfigDatum = DynamicConfigDatum
  { dcTallyValidator :: ValidatorHash
  -- ^ Hash of the `Triphut.Types.Tally.Script.validateTally` validator
  , dcTreasuryValidator :: ValidatorHash
  -- ^ Hash of the `Triphut.Types.Treasury.Script.validateTreasury` validator
  , dcConfigurationValidator :: ValidatorHash
  -- ^ Hash of the `Triphut.Types.ConfigurationNft.Script.validateConfiguration` validator
  , dcVoteValidator :: ValidatorHash
  -- ^ Hash of the `Triphut.Types.Vote.Script.validateVote` validator
  , dcUpgradeMajorityPercent :: Integer
  , dcUpgradeRelativeMajorityPercent :: Integer
  , dcGeneralMajorityPercent :: Integer
  , dcGeneralRelativeMajorityPercent :: Integer
  , dcTripMajorityPercent :: Integer
  , dcTripRelativeMajorityPercent :: Integer
  -- ^ The majority and relative majority percentages used
  -- in calculating whether a proposal has sufficient votes to pass
  -- (All times a 1000)
  , dcTotalVotes :: Integer
  -- ^ A threshold that needs to be passed when checking in
  -- the script if there is a sufficient relative majority
  , dcMaxGeneralDisbursement :: Integer
  , dcMaxTripDisbursement :: Integer
  -- ^ Disbursement allowable disbursement amounts, for general and trip proposals
  -- Checked in the `Triphut.Treasury.Script.validateTreasury` validator
  , dcAgentDisbursementPercent :: Integer
  -- ^ The percentage of the total travel cost for the agent in trip proposals
  -- Checked in the `Triphut.Treasury.Script.validateTreasury` validator
  -- (Agent disbursement percentage is times a 1000)
  , dcProposalTallyEndOffset :: Integer
  -- ^ Like a cool down period to be added to the proposal end time
  -- specified in the `Triphut.Types.TallyStateDatum` datum.
  -- The treasury cannot disburse funds until
  -- after the end time plus the offset has passed
  -- (Offset is in milliseconds)
  , dcTallyNft :: CurrencySymbol
  -- ^ Symbol of the `Triphut.Tally.Script.mkTallyNftMinter` minting policy
  , dcVoteCurrencySymbol :: CurrencySymbol
  -- ^ Symbol of the `Triphut.Vote.Script.mkVoteMinter` minting policy
  , dcVoteTokenName :: TokenName
  -- ^ Token name for the vote value minted by `Triphut.Vote.Script.mkVoteMinter`
  , dcVoteNft :: CurrencySymbol
  -- ^ Symbol of the Vote NFT
  , dcVoteFungibleCurrencySymbol :: CurrencySymbol
  -- ^ Symbol of the fungible vote value
  , dcVoteFungibleTokenName :: TokenName
  -- ^ Token name for fungible vote value
  , dcFungibleVotePercent :: Integer
  -- ^ Fungible token percentage (Percentage value is times a 1000)
  }

unstableMakeIsData ''DynamicConfigDatum
unstableMakeIsData ''TallyStateDatum
unstableMakeIsData ''ProposalType
