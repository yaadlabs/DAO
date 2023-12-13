{- |
Module      : Spec.Tally.SampleData
Description : Tally sample data for tests
-}
module Spec.Tally.SampleData (
  sampleUpgradeWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInPastTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum,
  sampleGeneralWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum,
) where

import Dao.Types (ProposalType (General, Trip, Upgrade), TallyStateDatum (..))
import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))
import Spec.Addresses (
  dummyGeneralPaymentAddress,
  dummyTravelAgentAddress,
  dummyTravelerPaymentAddress,
 )
import Spec.AlwaysSucceed.Script (alwaysSucceedCurrencySymbol)

sampleUpgradeWithEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeWithEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInPast
    , tsFor = 0
    , tsAgainst = 0
    }

sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = Trip dummyTravelAgentAddress dummyTravelerPaymentAddress 2
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 1
    , tsAgainst = 100
    }

sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 1
    , tsAgainst = 100
    }

sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInPast
    , tsFor = 1
    , tsAgainst = 100
    }

sampleUpgradeWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 0
    , tsAgainst = 0
    }

sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 8
    , tsAgainst = 4
    }

sampleTripWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleTripWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = Trip dummyTravelAgentAddress dummyTravelerPaymentAddress 2
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 5
    , tsAgainst = 3
    }

sampleUpgradeWithVotesEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeWithVotesEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInPast
    , tsFor = 5
    , tsAgainst = 3
    }

sampleGeneralWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleGeneralWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleGeneralProposalType
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 5
    , tsAgainst = 3
    }

sampleUpgradeProposalType :: ProposalType
sampleUpgradeProposalType = Upgrade alwaysSucceedCurrencySymbol

sampleGeneralProposalType :: ProposalType
sampleGeneralProposalType = General dummyGeneralPaymentAddress 1

-- Some arbitrary time way in the future
sampleEndTimeInFuture :: POSIXTime
sampleEndTimeInFuture = POSIXTime 3594201188000

sampleEndTimeInPast :: POSIXTime
sampleEndTimeInPast = 0
