module Spec.SampleData (
  sampleDynamicConfig,
  sampleTallyDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
) where

import Plutus.V1.Ledger.Value (TokenName (TokenName), adaSymbol, adaToken)
import Plutus.V2.Ledger.Api (ValidatorHash (ValidatorHash))
import Spec.Tally.Script (tallyValidatorHash')
import Spec.Values (dummyTallySymbol, dummyVoteFungibleSymbol)
import Spec.Vote.SampleData (sampleVoteMinterConfig)
import Spec.Vote.Script (voteCurrencySymbol, voteValidatorHash')
import Triphut.Tally (TallyDynamicConfigDatum (..))
import Triphut.Types (DynamicConfigDatum (..))

-- DynamicConfigDatum samples
sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig =
  DynamicConfigDatum
    { dcTallyNft = dummyTallySymbol
    , dcTallyValidator = tallyValidatorHash'
    , dcTreasuryValidator = ValidatorHash ""
    , dcConfigurationValidator = ValidatorHash ""
    , dcVoteCurrencySymbol = dummyVoteFungibleSymbol
    , dcVoteTokenName = TokenName "vote"
    , dcVoteValidator = voteValidatorHash'
    , dcUpgradeMajorityPercent = 1
    , dcUpgradeRelativeMajorityPercent = 1
    , dcGeneralMajorityPercent = 1
    , dcGeneralRelativeMajorityPercent = 20
    , dcTripMajorityPercent = 1
    , dcTripRelativeMajorityPercent = 1
    , dcVoteNft = voteCurrencySymbol sampleVoteMinterConfig
    , dcVoteFungibleCurrencySymbol = adaSymbol
    , dcVoteFungibleTokenName = adaToken
    , dcTotalVotes = 1
    , dcProposalTallyEndOffset = 0
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    , dcFungibleVotePercent = 1
    }

sampleHighRelativeMajorityHighTotalVotesDynamicConfig :: DynamicConfigDatum
sampleHighRelativeMajorityHighTotalVotesDynamicConfig =
  DynamicConfigDatum
    { dcTallyNft = dummyTallySymbol
    , dcTallyValidator = tallyValidatorHash'
    , dcTreasuryValidator = ValidatorHash ""
    , dcConfigurationValidator = ValidatorHash ""
    , dcVoteCurrencySymbol = dummyVoteFungibleSymbol
    , dcVoteTokenName = TokenName "vote"
    , dcVoteValidator = voteValidatorHash'
    , dcUpgradeMajorityPercent = 1
    , dcUpgradeRelativeMajorityPercent = 70
    , -- \^ Just set to high value for negative test for upgrading config
      dcGeneralMajorityPercent = 1
    , dcGeneralRelativeMajorityPercent = 1
    , dcTripMajorityPercent = 1
    , dcTripRelativeMajorityPercent = 1
    , dcVoteNft = voteCurrencySymbol sampleVoteMinterConfig
    , dcVoteFungibleCurrencySymbol = adaSymbol
    , dcVoteFungibleTokenName = adaToken
    , dcTotalVotes = 2000
    , -- \^ Set it high for negative test for upgrading config
      dcProposalTallyEndOffset = 0
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    , dcFungibleVotePercent = 1
    }

sampleTallyDynamicConfig :: TallyDynamicConfigDatum
sampleTallyDynamicConfig =
  TallyDynamicConfigDatum
    { tdcTallyNft = dummyTallySymbol
    , tdcVoteValidator = voteValidatorHash'
    , tdcVoteNft = voteCurrencySymbol sampleVoteMinterConfig
    , tdcVoteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , tdcVoteFungibleTokenName = adaToken
    , tdcFungibleVotePercent = 1
    }
