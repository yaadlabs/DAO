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
    { vmdcTallyNft = dummyTallySymbol
    , vmdcVoteTokenName = TokenName "vote"
    , vmdcVoteValidator = voteValidatorHash'
    , vmdcVoteNft = voteCurrencySymbol sampleVoteMinterConfig
    }

-- VoteDynamicConfigDatum samples
sampleVoteDynamicConfig :: VoteDynamicConfigDatum
sampleVoteDynamicConfig =
  VoteDynamicConfigDatum
    { vdcTallyValidator = tallyValidatorHash'
    , vdcVoteCurrencySymbol = toBuiltinData (dummyVoteFungibleSymbol :: CurrencySymbol)
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
