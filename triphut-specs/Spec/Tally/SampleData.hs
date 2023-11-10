{- |
Module      : Spec.Tally.SampleData
Description : Tally sample data for tests
-}
module Spec.Tally.SampleData (
  sampleTallyValidatorConfig,
  sampleTallyStateDatum,
) where

import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Value (adaSymbol, adaToken)
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
    { tvcConfigNftCurrencySymbol = adaSymbol
    , tvcConfigNftTokenName = adaToken
    }

sampleTallyStateDatum :: TallyStateDatum
sampleTallyStateDatum =
  TallyStateDatum
    { tsProposal = sampleUpgradeProposalType
    , tsProposalEndTime = sampleEndTime
    , tsFor = 3
    , tsAgainst = 2
    }

sampleUpgradeProposalType :: ProposalType
sampleUpgradeProposalType = Upgrade adaSymbol

sampleEndTime :: POSIXTime
sampleEndTime = POSIXTime 0
