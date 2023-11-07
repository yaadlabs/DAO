{- |
Module      : Spec.ConfigurationNft.SampleData
Description : ConfigurationNft sample data for tests
-}
module Spec.ConfigurationNft.SampleData (
  sampleDynamicConfig,
  sampleConfigValidatorConfig,
) where

import Plutus.V1.Ledger.Value (adaSymbol, adaToken)
import Plutus.V2.Ledger.Api (ValidatorHash (ValidatorHash))
import Triphut.ConfigurationNft (ConfigurationValidatorConfig (..))
import Triphut.Types (DynamicConfig (..))

sampleDynamicConfig :: DynamicConfig
sampleDynamicConfig =
  DynamicConfig
    { dcTallyNft = adaSymbol
    , dcTallyValidator = ValidatorHash ""
    , dcUpgradeMajorityPercent = 1
    , dcUpgradeRelativeMajorityPercent = 1
    , dcGeneralMajorityPercent = 1
    , dcGeneralRelativeMajorityPercent = 1
    , dcTripMajorityPercent = 1
    , dcTripRelativeMajorityPercent = 1
    , dcTotalVotes = 1
    , dcProposalTallyEndOffset = 1
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    }

sampleConfigValidatorConfig :: ConfigurationValidatorConfig
sampleConfigValidatorConfig =
  ConfigurationValidatorConfig
    { cvcConfigNftCurrencySymbol = adaSymbol
    , cvcConfigNftTokenName = adaToken
    }
