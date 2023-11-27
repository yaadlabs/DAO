module Spec.SampleData (
  sampleDynamicConfig,
  sampleVoteMinterDynamicConfig,
  sampleVoteDynamicConfig,
  sampleTallyDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
) where

import Dao.Tally (TallyDynamicConfigDatum (..))
import Dao.Types (DynamicConfigDatum (..))
import Dao.Vote (
  VoteDynamicConfigDatum (..),
  VoteMinterDynamicConfigDatum (..),
 )
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (TokenName), adaSymbol, adaToken)
import Plutus.V2.Ledger.Api (ValidatorHash (ValidatorHash))
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude (Integer)
import Spec.Tally.Script (tallyValidatorHash')
import Spec.Values (dummyTallySymbol, dummyVoteFungibleSymbol)
import Spec.Vote.SampleData (sampleVoteMinterConfig)
import Spec.Vote.Script (voteCurrencySymbol, voteValidatorHash')

-- DynamicConfigDatum samples
sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig =
  DynamicConfigDatum
    { dcTallyNft = dummyTallySymbol
    , dcTallyValidator = tallyValidatorHash'
    , dcTreasuryValidator = ValidatorHash ""
    , dcConfigurationValidator = ValidatorHash ""
    , dcVoteCurrencySymbol = adaSymbol
    , dcVoteTokenName = adaToken
    , dcVoteValidator = ValidatorHash ""
    , dcUpgradeMajorityPercent = 1
    , dcUpgradeRelativeMajorityPercent = 1
    , dcGeneralMajorityPercent = 1
    , dcGeneralRelativeMajorityPercent = 20
    , dcTripMajorityPercent = 1
    , dcTripRelativeMajorityPercent = 1
    , dcVoteNft = adaSymbol
    , dcVoteFungibleCurrencySymbol = adaSymbol
    , dcVoteFungibleTokenName = adaToken
    , dcTotalVotes = 1
    , dcProposalTallyEndOffset = 0
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    , dcFungibleVotePercent = 1
    , dcTallyIndexNft = adaSymbol
    }

sampleHighRelativeMajorityHighTotalVotesDynamicConfig :: DynamicConfigDatum
sampleHighRelativeMajorityHighTotalVotesDynamicConfig =
  DynamicConfigDatum
    { dcTallyNft = dummyTallySymbol
    , dcTallyValidator = tallyValidatorHash'
    , dcTreasuryValidator = ValidatorHash ""
    , dcConfigurationValidator = ValidatorHash ""
    , dcVoteCurrencySymbol = adaSymbol
    , dcVoteTokenName = adaToken
    , dcVoteValidator = ValidatorHash ""
    , dcUpgradeMajorityPercent = 1
    , dcUpgradeRelativeMajorityPercent = 70
    , -- \^ Just set to high value for negative test for upgrading config
      dcGeneralMajorityPercent = 1
    , dcGeneralRelativeMajorityPercent = 1
    , dcTripMajorityPercent = 1
    , dcTripRelativeMajorityPercent = 1
    , dcVoteNft = adaSymbol
    , dcVoteFungibleCurrencySymbol = adaSymbol
    , dcVoteFungibleTokenName = adaToken
    , dcTotalVotes = 2000
    , -- \^ Set it high for negative test for upgrading config
      dcProposalTallyEndOffset = 0
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    , dcFungibleVotePercent = 1
    , dcTallyIndexNft = adaSymbol
    }

-- VoteMinterDynamicConfigDatum samples
sampleVoteMinterDynamicConfig :: VoteMinterDynamicConfigDatum
sampleVoteMinterDynamicConfig =
  VoteMinterDynamicConfigDatum
    { vmdcTallyIndexNft = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vmdcTallyNft = dummyTallySymbol
    , vmdcTallyValidator = toBuiltinData tallyValidatorHash'
    , vmdcTreasuryValidator = toBuiltinData (ValidatorHash "")
    , vmdcConfigurationValidator = toBuiltinData (ValidatorHash "")
    , vmdcVoteCurrencySymbol = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vmdcVoteTokenName = TokenName "vote"
    , vmdcVoteValidator = voteValidatorHash'
    , vmdcUpgradeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcUpgradRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcGeneralMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcGeneralRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcTripMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcTripRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcTotalVotes = toBuiltinData (1 :: Integer)
    , vmdcVoteNft = voteCurrencySymbol sampleVoteMinterConfig
    , vmdcVoteFungibleCurrencySymbol = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vmdcVoteFungibleTokenName = toBuiltinData (adaToken :: TokenName)
    , vmdcProposalTallyEndOffset = toBuiltinData (1 :: Integer)
    , vmdcMaxGeneralDisbursement = toBuiltinData (1 :: Integer)
    , vmdcMaxTripDisbursement = toBuiltinData (1 :: Integer)
    , vmdcAgentDisbursementPercent = toBuiltinData (1 :: Integer)
    , vmdcFungibleVotePercent = toBuiltinData (1 :: Integer)
    }

-- VoteDynamicConfigDatum samples
sampleVoteDynamicConfig :: VoteDynamicConfigDatum
sampleVoteDynamicConfig =
  VoteDynamicConfigDatum
    { vdcTallyIndexNft = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vdcTallyNft = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vdcTallyValidator = tallyValidatorHash'
    , vdcTreasuryValidator = toBuiltinData (ValidatorHash "")
    , vdcConfigurationValidator = toBuiltinData (ValidatorHash "")
    , vdcVoteCurrencySymbol = toBuiltinData (dummyVoteFungibleSymbol :: CurrencySymbol)
    , vdcVoteTokenName = toBuiltinData (adaToken :: TokenName)
    , vdcVoteValidator = toBuiltinData (ValidatorHash "")
    , vdcUpgradeMajorityPercent = toBuiltinData (1 :: Integer)
    , vdcUpgradRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vdcGeneralMajorityPercent = toBuiltinData (1 :: Integer)
    , vdcGeneralRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vdcTripMajorityPercent = toBuiltinData (1 :: Integer)
    , vdcTripRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vdcTotalVotes = toBuiltinData (1 :: Integer)
    , vdcVoteNft = toBuiltinData adaSymbol
    , vdcVoteFungibleCurrencySymbol = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vdcVoteFungibleTokenName = toBuiltinData (adaToken :: TokenName)
    , vdcProposalTallyEndOffset = toBuiltinData (1 :: Integer)
    , vdcMaxGeneralDisbursement = toBuiltinData (1 :: Integer)
    , vdcMaxTripDisbursement = toBuiltinData (1 :: Integer)
    , vdcAgentDisbursementPercent = toBuiltinData (1 :: Integer)
    , vdcFungibleVotePercent = toBuiltinData (1 :: Integer)
    }

sampleTallyDynamicConfig :: TallyDynamicConfigDatum
sampleTallyDynamicConfig =
  TallyDynamicConfigDatum
    { tdcTallyIndexNft = toBuiltinData (adaSymbol :: CurrencySymbol)
    , tdcTallyNft = dummyTallySymbol
    , tdcTallyValidator = toBuiltinData tallyValidatorHash'
    , tdcTreasuryValidator = toBuiltinData (ValidatorHash "")
    , tdcConfigurationValidator = toBuiltinData (ValidatorHash "")
    , tdcVoteCurrencySymbol = adaSymbol
    , tdcVoteTokenName = toBuiltinData (TokenName "")
    , tdcVoteValidator = voteValidatorHash'
    , tdcUpgradeMajorityPercent = toBuiltinData (1 :: Integer)
    , tdcUpgradRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , tdcGeneralMajorityPercent = toBuiltinData (1 :: Integer)
    , tdcGeneralRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , tdcTripMajorityPercent = toBuiltinData (1 :: Integer)
    , tdcTripRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , tdcTotalVotes = toBuiltinData (1 :: Integer)
    , tdcVoteNft = voteCurrencySymbol sampleVoteMinterConfig
    , tdcVoteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , tdcVoteFungibleTokenName = adaToken
    , tdcProposalTallyEndOffset = toBuiltinData (1 :: Integer)
    , tdcMaxGeneralDisbursement = toBuiltinData (1 :: Integer)
    , tdcMaxTripDisbursement = toBuiltinData (1 :: Integer)
    , tdcAgentDisbursementPercent = toBuiltinData (1 :: Integer)
    , tdcFungibleVotePercent = 1
    }
