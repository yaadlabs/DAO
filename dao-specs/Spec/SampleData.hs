module Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
) where

import Dao.Types (DynamicConfigDatum (..))
import Plutus.V1.Ledger.Value (TokenName (TokenName), adaToken)
import Plutus.V2.Ledger.Api (ValidatorHash (ValidatorHash))
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.Tally.Script (tallyValidatorHash')
import Spec.Values (dummyTallySymbol, dummyVoteFungibleSymbol)
import Spec.Vote.Script (voteCurrencySymbol, voteValidatorHash')

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
    , dcVoteNft = voteCurrencySymbol sampleConfigValidatorConfig
    , dcVoteFungibleCurrencySymbol = dummyVoteFungibleSymbol
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
    , dcVoteNft = voteCurrencySymbol sampleConfigValidatorConfig
    , dcVoteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , dcVoteFungibleTokenName = adaToken
    , dcTotalVotes = 2000
    , -- \^ Set it high for negative test for upgrading config
      dcProposalTallyEndOffset = 0
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    , dcFungibleVotePercent = 1
    }
