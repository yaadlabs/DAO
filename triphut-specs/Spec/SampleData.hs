module Spec.SampleData (
  sampleDynamicConfig,
  sampleVoteDynamicConfig,
) where

import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, adaSymbol, adaToken)
import Plutus.V2.Ledger.Api (ValidatorHash (ValidatorHash))
import PlutusTx (toBuiltinData)
import PlutusTx.Prelude (Integer)
import Spec.Tally.Script (tallyValidatorHash')
import Spec.Vote.Script (voteValidatorHash')
import Triphut.Types (DynamicConfigDatum (..))
import Triphut.Vote (VoteMinterDynamicConfigDatum (..))

-- DynamicConfigDatum samples
sampleDynamicConfig :: DynamicConfigDatum
sampleDynamicConfig =
  DynamicConfigDatum
    { dcTallyNft = adaSymbol
    , dcTallyValidator = tallyValidatorHash'
    , dcTreasuryValidator = ValidatorHash ""
    , dcConfigurationValidator = ValidatorHash ""
    , dcVoteCurrencySymbol = adaSymbol
    , dcVoteTokenName = adaToken
    , dcVoteValidator = ValidatorHash ""
    , dcUpgradeMajorityPercent = 1
    , dcUpgradeRelativeMajorityPercent = 1
    , dcGeneralMajorityPercent = 1
    , dcGeneralRelativeMajorityPercent = 1
    , dcTripMajorityPercent = 1
    , dcTripRelativeMajorityPercent = 1
    , dcVoteNft = adaSymbol
    , dcVoteFungibleCurrencySymbol = adaSymbol
    , dcVoteFungibleTokenName = adaToken
    , dcTotalVotes = 1
    , dcProposalTallyEndOffset = 1
    , dcMaxGeneralDisbursement = 1
    , dcMaxTripDisbursement = 1
    , dcAgentDisbursementPercent = 1
    , dcFungibleVotePercent = 1
    , dcTallyIndexNft = adaSymbol
    }

-- VoteMinterDynamicConfigDatum samples
sampleVoteDynamicConfig :: VoteMinterDynamicConfigDatum
sampleVoteDynamicConfig =
  VoteMinterDynamicConfigDatum
    { vmdcTallyIndexNft = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vmdcTallyNft = adaSymbol
    , vmdcTallyValidator = toBuiltinData (tallyValidatorHash')
    , vmdcTreasuryValidator = toBuiltinData (ValidatorHash "")
    , vmdcConfigurationValidator = toBuiltinData (ValidatorHash "")
    , vmdcVoteCurrencySymbol = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vmdcVoteTokenName = adaToken
    , vmdcVoteValidator = voteValidatorHash'
    , vmdcUpgradeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcUpgradRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcGeneralMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcGeneralRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcTripMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcTripRelativeMajorityPercent = toBuiltinData (1 :: Integer)
    , vmdcTotalVotes = toBuiltinData (1 :: Integer)
    , vmdcVoteNft = adaSymbol
    , vmdcVoteFungibleCurrencySymbol = toBuiltinData (adaSymbol :: CurrencySymbol)
    , vmdcVoteFungibleTokenName = toBuiltinData (adaToken :: TokenName)
    , vmdcProposalTallyEndOffset = toBuiltinData (1 :: Integer)
    , vmdcMaxGeneralDisbursement = toBuiltinData (1 :: Integer)
    , vmdcMaxTripDisbursement = toBuiltinData (1 :: Integer)
    , vmdcAgentDisbursementPercent = toBuiltinData (1 :: Integer)
    , vmdcFungibleVotePercent = toBuiltinData (1 :: Integer)
    }
