module Spec.ConfigurationNft.Data 

data DynamicConfig = DynamicConfig
  { dcTallyNft :: CurrencySymbol
  , dcTallyValidator :: ValidatorHash
  , dcUpgradeMajorityPercent :: Integer -- times a 1000
  , dcUpgradeRelativeMajorityPercent :: Integer -- times a 1000
  , dcGeneralMajorityPercent :: Integer -- times a 1000
  , dcGeneralRelativeMajorityPercent :: Integer -- times a 1000
  , dcTripMajorityPercent :: Integer -- times a 1000
  , dcTripRelativeMajorityPercent :: Integer -- times a 1000
  , dcTotalVotes :: Integer
  , dcProposalTallyEndOffset :: Integer -- in milliseconds
  , dcMaxGeneralDisbursement :: Integer
  , dcMaxTripDisbursement :: Integer
  , dcAgentDisbursementPercent :: Integer -- times a 1000
  }
