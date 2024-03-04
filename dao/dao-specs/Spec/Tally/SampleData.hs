{- |
Module      : Spec.Tally.SampleData
Description : Tally sample data for tests
-}
module Spec.Tally.SampleData (
  sampleUpgradeWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInPastTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleTripWithEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum,
  sampleGeneralWithEndTimeInFutureTallyStateDatum,
  sampleGeneralWithEndTimeInPastTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum,
  sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum,
) where

import LambdaBuffers.ApplicationTypes.Proposal (
  ProposalType (
    ProposalType'General,
    ProposalType'Trip,
    ProposalType'Upgrade
  ),
 )
import LambdaBuffers.ApplicationTypes.Tally (
  TallyStateDatum (
    TallyStateDatum,
    tallyStateDatum'against,
    tallyStateDatum'for,
    tallyStateDatum'proposal,
    tallyStateDatum'proposalEndTime
  ),
 )
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime))
import Spec.Addresses (
  dummyGeneralPaymentAddress,
  dummyTravelAgentAddress,
  dummyTravelerPaymentAddress,
 )
import Spec.AlwaysSucceed.Script (alwaysSucceedCurrencySymbol)

sampleUpgradeWithEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeWithEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleUpgradeProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInPast
    , tallyStateDatum'for = 0
    , tallyStateDatum'against = 0
    }

sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleTripNotEnoughVotesEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = ProposalType'Trip dummyTravelAgentAddress dummyTravelerPaymentAddress 2
    , tallyStateDatum'proposalEndTime = sampleEndTimeInFuture
    , tallyStateDatum'for = 1
    , tallyStateDatum'against = 100
    }

sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleTripNotEnoughVotesEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = ProposalType'Trip dummyTravelAgentAddress dummyTravelerPaymentAddress 2
    , tallyStateDatum'proposalEndTime = sampleEndTimeInPast
    , tallyStateDatum'for = 1
    , tallyStateDatum'against = 100
    }

sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeNotEnoughVotesEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleUpgradeProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInFuture
    , tallyStateDatum'for = 1
    , tallyStateDatum'against = 100
    }

sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleUpgradeProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInPast
    , tallyStateDatum'for = 1
    , tallyStateDatum'against = 100
    }

sampleUpgradeWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleUpgradeProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInFuture
    , tallyStateDatum'for = 0
    , tallyStateDatum'against = 0
    }

sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeWithVotesEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleUpgradeProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInFuture
    , tallyStateDatum'for = 8
    , tallyStateDatum'against = 4
    }

sampleTripWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleTripWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = ProposalType'Trip dummyTravelAgentAddress dummyTravelerPaymentAddress 2
    , tallyStateDatum'proposalEndTime = sampleEndTimeInFuture
    , tallyStateDatum'for = 5
    , tallyStateDatum'against = 3
    }

sampleTripWithEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleTripWithEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = ProposalType'Trip dummyTravelAgentAddress dummyTravelerPaymentAddress 2
    , tallyStateDatum'proposalEndTime = sampleEndTimeInPast
    , tallyStateDatum'for = 5
    , tallyStateDatum'against = 3
    }

sampleUpgradeWithVotesEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeWithVotesEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleUpgradeProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInPast
    , tallyStateDatum'for = 5
    , tallyStateDatum'against = 3
    }

sampleGeneralWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleGeneralWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleGeneralProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInFuture
    , tallyStateDatum'for = 5
    , tallyStateDatum'against = 3
    }

sampleGeneralWithEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleGeneralWithEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tallyStateDatum'proposal = sampleGeneralProposalType
    , tallyStateDatum'proposalEndTime = sampleEndTimeInPast
    , tallyStateDatum'for = 5
    , tallyStateDatum'against = 3
    }

sampleUpgradeProposalType :: ProposalType
sampleUpgradeProposalType = ProposalType'Upgrade alwaysSucceedCurrencySymbol

sampleGeneralProposalType :: ProposalType
sampleGeneralProposalType = ProposalType'General dummyGeneralPaymentAddress 1

-- Some arbitrary time way in the future
sampleEndTimeInFuture :: POSIXTime
sampleEndTimeInFuture = POSIXTime 3594201188000

sampleEndTimeInPast :: POSIXTime
sampleEndTimeInPast = 0
