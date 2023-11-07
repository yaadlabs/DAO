module Spec.ConfigurationNft.SampleData 
  ( sampleDynamicConfig
  , sampleConfigValidatorConfig 
  ) where

import Triphut.Types (DynamicConfig(..))
import Triphut.ConfigurationNft (ConfigurationValidatorConfig(..))
import Plutus.V2.Ledger.Api (ValidatorHash(ValidatorHash))
import Plutus.V1.Ledger.Value (adaSymbol, adaToken)

sampleDynamicConfig :: DynamicConfig
sampleDynamicConfig = DynamicConfig
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
sampleConfigValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol = adaSymbol
  , cvcConfigNftTokenName = adaToken
  }
