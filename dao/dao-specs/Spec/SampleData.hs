module Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
) where

import LambdaBuffers.ApplicationTypes.Configuration (DynamicConfigDatum (..))
import PlutusLedgerApi.V1.Scripts (ScriptHash (ScriptHash))
import PlutusLedgerApi.V1.Value (TokenName (TokenName), adaToken)
import Spec.Configuration.SampleData (sampleConfigValidatorConfig)
import Spec.Tally.Script (tallyValidatorScriptHash)
import Spec.Treasury.Script (treasuryValidatorScriptHash)
import Spec.Values (dummyTallySymbol, dummyVoteFungibleSymbol)
import Spec.Vote.Script (voteCurrencySymbol, voteValidatorScriptHash)

-- DynamicConfigDatum samples
sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig =
  DynamicConfigDatum
    { dynamicConfigDatum'treasuryValidator = treasuryValidatorScriptHash
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

sampleHighRelativeMajorityHighTotalVotesDynamicConfig :: DynamicConfigDatum
sampleHighRelativeMajorityHighTotalVotesDynamicConfig =
  DynamicConfigDatum
    { dynamicConfigDatum'treasuryValidator = treasuryValidatorScriptHash
    , dynamicConfigDatum'tallyValidator = tallyValidatorScriptHash
    , dynamicConfigDatum'configurationValidator = ScriptHash ""
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
    , dynamicConfigDatum'tallyNft = dummyTallySymbol
    , dynamicConfigDatum'voteCurrencySymbol = dummyVoteFungibleSymbol
    , dynamicConfigDatum'voteTokenName = TokenName "vote"
    , dynamicConfigDatum'voteNft = voteCurrencySymbol sampleConfigValidatorConfig
    , dynamicConfigDatum'voteFungibleCurrencySymbol = dummyVoteFungibleSymbol
    , dynamicConfigDatum'voteFungibleTokenName = adaToken
    , dynamicConfigDatum'fungibleVotePercent = 1
    }
