{- |
Module      : Spec.Tally.SampleData
Description : Tally sample data for tests
-}
module Spec.Tally.SampleData (
  sampleTallyValidatorConfig,
  sampleWithEndTimeInFutureTallyStateDatum,
  sampleWithEndTimeInPastTallyStateDatum,
) where

import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Value (adaSymbol)
import Spec.Values (dummyTallyConfigSymbol, dummyTallyConfigTokenName)
import Triphut.Tally (
  TallyValidatorConfig (
    TallyValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
 )
import Triphut.Types (ProposalType (Upgrade), TallyStateDatum (..))

-- | Sample tally config
sampleTallyValidatorConfig :: TallyValidatorConfig
sampleTallyValidatorConfig =
  TallyValidatorConfig
    { tvcConfigNftCurrencySymbol = dummyTallyConfigSymbol
    , tvcConfigNftTokenName = dummyTallyConfigTokenName
    }

sampleWithEndTimeInPastTallyStateDatum :: TallyStateDatum
sampleWithEndTimeInPastTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInPast
    , tsFor = 0
    , tsAgainst = 0
    }

sampleWithEndTimeInFutureTallyStateDatum :: TallyStateDatum
sampleWithEndTimeInFutureTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTimeInFuture
    , tsFor = 0
    , tsAgainst = 0
    }

sampleUpgradeProposalType :: ProposalType
sampleUpgradeProposalType = Upgrade adaSymbol

-- Some arbitrary time way in the future
sampleEndTimeInFuture :: POSIXTime
sampleEndTimeInFuture = POSIXTime 3594201188000

sampleEndTimeInPast :: POSIXTime
sampleEndTimeInPast = 0
