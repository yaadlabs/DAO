module Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
) where

-- import Dao.Types (DynamicConfigDatum (..))
import LambdaBuffers.Types.Configuration (DynamicConfigDatum(..))
import PlutusLedgerApi.V1.Scripts (ScriptHash (ScriptHash))
import PlutusLedgerApi.V1.Value (TokenName (TokenName), adaToken)
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.Tally.Script (tallyValidatorScriptHash)
import Spec.Values (dummyTallySymbol, dummyVoteFungibleSymbol)
import Spec.Vote.Script (voteCurrencySymbol, voteValidatorScriptHash)
import Spec.Treasury.Script (treasuryValidatorScriptHash)
import Prelude (undefined)

sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig =
  DynamicConfigDatum {
      dynamicConfigDatum'treasuryValidator = treasuryValidatorScriptHash
    , dynamicConfigDatum'tallyValidator = tallyValidatorScriptHash
    , dynamicConfigDatum'configurationValidator = ScriptHash ""                  
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
    , dynamicConfigDatum'tallyNft = dummyTallySymbol
    , dynamicConfigDatum'voteCurrencySymbol = dummyVoteFungibleSymbol
    , dynamicConfigDatum'voteTokenName = TokenName "vote"
    , dynamicConfigDatum'voteNft = voteCurrencySymbol sampleConfigValidatorConfig
    , dynamicConfigDatum'voteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , dynamicConfigDatum'voteFungibleTokenName = adaToken
    , dynamicConfigDatum'fungibleVotePercent = 1
  }

-- DynamicConfigDatum samples
-- sampleDynamicConfig :: DynamicConfigDatum
-- sampleDynamicConfig =
--   DynamicConfigDatum
--     { dcTallyNft = dummyTallySymbol
--     , dcTallyValidator = tallyValidatorScriptHash
--     , dcTreasuryValidator = ScriptHash ""
--     , dcConfigurationValidator = ScriptHash ""
--     , dcVoteCurrencySymbol = dummyVoteFungibleSymbol
--     , dcVoteTokenName = TokenName "vote"
--     , dcVoteValidator = voteValidatorScriptHash
--     , dcUpgradeMajorityPercent = 1
--     , dcUpgradeRelativeMajorityPercent = 1
--     , dcGeneralMajorityPercent = 1
--     , dcGeneralRelativeMajorityPercent = 20
--     , dcTripMajorityPercent = 1
--     , dcTripRelativeMajorityPercent = 1
--     , dcVoteNft = voteCurrencySymbol sampleConfigValidatorConfig
--     , dcVoteFungibleCurrencySymbol = dummyVoteFungibleSymbol
--     , dcVoteFungibleTokenName = adaToken
--     , dcTotalVotes = 1
--     , dcProposalTallyEndOffset = 0
--     , dcMaxGeneralDisbursement = 1
--     , dcMaxTripDisbursement = 1
--     , dcAgentDisbursementPercent = 1
--     , dcFungibleVotePercent = 1
--     }

sampleHighRelativeMajorityHighTotalVotesDynamicConfig = undefined

-- sampleHighRelativeMajorityHighTotalVotesDynamicConfig :: DynamicConfigDatum
-- sampleHighRelativeMajorityHighTotalVotesDynamicConfig =
--   DynamicConfigDatum
--     { dcTallyNft = dummyTallySymbol
--     , dcTallyValidator = tallyValidatorScriptHash
--     , dcTreasuryValidator = ScriptHash ""
--     , dcConfigurationValidator = ScriptHash ""
--     , dcVoteCurrencySymbol = dummyVoteFungibleSymbol
--     , dcVoteTokenName = TokenName "vote"
--     , dcVoteValidator = voteValidatorScriptHash
--     , dcUpgradeMajorityPercent = 1
--     , dcUpgradeRelativeMajorityPercent = 70
--     , -- \^ Just set to high value for negative test for upgrading config
--       dcGeneralMajorityPercent = 1
--     , dcGeneralRelativeMajorityPercent = 1
--     , dcTripMajorityPercent = 1
--     , dcTripRelativeMajorityPercent = 1
--     , dcVoteNft = voteCurrencySymbol sampleConfigValidatorConfig
--     , dcVoteFungibleCurrencySymbol = dummyVoteFungibleSymbol
--     , dcVoteFungibleTokenName = adaToken
--     , dcTotalVotes = 2000
--     , -- \^ Set it high for negative test for upgrading config
--       dcProposalTallyEndOffset = 0
--     , dcMaxGeneralDisbursement = 1
--     , dcMaxTripDisbursement = 1
--     , dcAgentDisbursementPercent = 1
--     , dcFungibleVotePercent = 1
--     }
