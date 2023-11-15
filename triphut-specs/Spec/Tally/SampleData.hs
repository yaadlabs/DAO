{- |
Module      : Spec.Tally.SampleData
Description : Tally sample data for tests
-}
module Spec.Tally.SampleData (
  sampleTallyValidatorConfig,
  sampleUpgradeWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithEndTimeInPastTallyStateDatum,
  sampleTripWithEndTimeInFutureTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
) where

import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))
import Spec.Addresses (
  sampleTravelAgentAddress,
  sampleTravelerPaymentAddress,
 )
import Spec.AlwaysSucceed.Script (alwaysSucceedCurrencySymbol)
import Spec.Values (dummyTallyConfigSymbol, dummyTallyConfigTokenName)
import Triphut.Tally (
  TallyValidatorConfig (
    TallyValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
 )
import Triphut.Types (ProposalType (Trip, Upgrade), TallyStateDatum (..))

-- | Sample tally config
sampleTallyValidatorConfig :: TallyValidatorConfig
sampleTallyValidatorConfig =
  TallyValidatorConfig
    { tvcConfigNftCurrencySymbol = dummyTallyConfigSymbol
    , tvcConfigNftTokenName = dummyTallyConfigTokenName
    }

sampleUpgradeWithEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleUpgradeWithEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInPast
    , tsFor = 0
    , tsAgainst = 0
    }

sampleUpgradeWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleUpgradeWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 0
    , tsAgainst = 0
    }

sampleTripWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleTripWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = Trip sampleTravelAgentAddress sampleTravelerPaymentAddress 2
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

sampleUpgradeProposalType :: ProposalType
sampleUpgradeProposalType = Upgrade alwaysSucceedCurrencySymbol

-- Some arbitrary time way in the future
sampleEndTimeInFuture :: POSIXTime
sampleEndTimeInFuture = POSIXTime 3594201188000

sampleEndTimeInPast :: POSIXTime
sampleEndTimeInPast = 0
