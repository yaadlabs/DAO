module Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
  sampleTreasuryTestDynamicConfig,
  sampleHighThresholdTreasuryTestDynamicConfig,
  sampleTallyPolicyParams,
) where

import Dao.ScriptArgument (TallyPolicyParams (TallyPolicyParams))
import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum (..))
import PlutusLedgerApi.V1.Value (TokenName (TokenName), adaToken)
import Spec.Addresses (dummyGeneralPaymentAddress)
import Spec.AlwaysSucceed.Script (alwaysSucceedCurrencySymbol)
import Spec.Configuration.SampleData (sampleValidatorParams)
import Spec.Configuration.Script (configurationValidatorScriptHash)
import Spec.Tally.Script (tallyConfigNftCurrencySymbol, tallyValidatorScriptHash)
import Spec.Treasury.Script (treasuryValidatorScriptHash)
import Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyIndexConfigNftSymbol,
  dummyIndexConfigNftTokenName,
  dummyTallySymbol,
  dummyVoteFungibleSymbol,
  dummyVoteNFTSymbol,
 )
import Spec.Vote.Script (voteCurrencySymbol, voteValidatorScriptHash)

-- Shared tally policy params used across tests
sampleTallyPolicyParams :: TallyPolicyParams
sampleTallyPolicyParams =
  TallyPolicyParams
    dummyIndexConfigNftSymbol
    dummyIndexConfigNftTokenName
    dummyConfigNftSymbol
    dummyConfigNftTokenName

-- DynamicConfigDatum samples
sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig =
  DynamicConfigDatum
    { dynamicConfigDatum'treasuryValidator = treasuryValidatorScriptHash
    , dynamicConfigDatum'tallyValidator = tallyValidatorScriptHash
    , dynamicConfigDatum'configurationValidator = configurationValidatorScriptHash
    , dynamicConfigDatum'voteValidator = voteValidatorScriptHash
    , dynamicConfigDatum'upgradeMajorityPercent = 1
    , dynamicConfigDatum'upgradeRelativeMajorityPercent = 1
    , dynamicConfigDatum'generalMajorityPercent = 1
    , dynamicConfigDatum'generalRelativeMajorityPercent = 20
    , dynamicConfigDatum'tripMajorityPercent = 1
    , dynamicConfigDatum'tripRelativeMajorityPercent = 1
    , dynamicConfigDatum'totalVotes = 1
    , dynamicConfigDatum'maxGeneralDisbursement = 1
    , dynamicConfigDatum'maxTripDisbursement = 1
    , dynamicConfigDatum'agentDisbursementPercent = 1
    , dynamicConfigDatum'proposalTallyEndOffset = 0
    , dynamicConfigDatum'tallyNft = dummyTallySymbol -- Use dummy for simplified tests
    , dynamicConfigDatum'voteCurrencySymbol = voteCurrencySymbol sampleValidatorParams
    , dynamicConfigDatum'voteTokenName = TokenName "vote"
    , dynamicConfigDatum'voteNft = dummyVoteNFTSymbol
    , dynamicConfigDatum'voteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , dynamicConfigDatum'voteFungibleTokenName = adaToken
    , dynamicConfigDatum'fungibleVotePercent = 1
    , dynamicConfigDatum'minTreasuryValue = 3_000_000
    , dynamicConfigDatum'protocolFallbackAddress = dummyGeneralPaymentAddress
    }

sampleHighRelativeMajorityHighTotalVotesDynamicConfig :: DynamicConfigDatum
sampleHighRelativeMajorityHighTotalVotesDynamicConfig =
  DynamicConfigDatum
    { dynamicConfigDatum'treasuryValidator = treasuryValidatorScriptHash
    , dynamicConfigDatum'tallyValidator = tallyValidatorScriptHash
    , dynamicConfigDatum'configurationValidator = configurationValidatorScriptHash
    , dynamicConfigDatum'voteValidator = voteValidatorScriptHash
    , dynamicConfigDatum'upgradeMajorityPercent = 1
    , dynamicConfigDatum'upgradeRelativeMajorityPercent = 70
    , -- \^ Just set to high value for negative test for upgrading config
      dynamicConfigDatum'generalMajorityPercent = 1
    , dynamicConfigDatum'generalRelativeMajorityPercent = 20
    , dynamicConfigDatum'tripMajorityPercent = 1
    , dynamicConfigDatum'tripRelativeMajorityPercent = 1
    , dynamicConfigDatum'totalVotes = 2000
    , dynamicConfigDatum'maxGeneralDisbursement = 1
    , dynamicConfigDatum'maxTripDisbursement = 1
    , dynamicConfigDatum'agentDisbursementPercent = 1
    , dynamicConfigDatum'proposalTallyEndOffset = 0
    , dynamicConfigDatum'tallyNft = dummyTallySymbol -- Use dummy for simplified tests
    , dynamicConfigDatum'voteCurrencySymbol = voteCurrencySymbol sampleValidatorParams
    , dynamicConfigDatum'voteTokenName = TokenName "vote"
    , dynamicConfigDatum'voteNft = dummyVoteNFTSymbol
    , dynamicConfigDatum'voteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , dynamicConfigDatum'voteFungibleTokenName = adaToken
    , dynamicConfigDatum'fungibleVotePercent = 1
    , dynamicConfigDatum'minTreasuryValue = 3_000_000
    , dynamicConfigDatum'protocolFallbackAddress = dummyGeneralPaymentAddress
    }

-- Special config for Treasury tests that uses alwaysSucceed currency for tally burning (ID-501)
sampleTreasuryTestDynamicConfig :: DynamicConfigDatum
sampleTreasuryTestDynamicConfig =
  sampleDynamicConfig
    { dynamicConfigDatum'tallyNft = alwaysSucceedCurrencySymbol -- Use alwaysSucceed so we can burn
    }

-- Special config for negative Treasury tests (high threshold + alwaysSucceed tally)
sampleHighThresholdTreasuryTestDynamicConfig :: DynamicConfigDatum
sampleHighThresholdTreasuryTestDynamicConfig =
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig
    { dynamicConfigDatum'tallyNft = alwaysSucceedCurrencySymbol -- Use alwaysSucceed so we can burn
    }
