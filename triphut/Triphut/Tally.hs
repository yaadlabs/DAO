{- |
Module: Triphut.Tally
Description: Contains all the tally specific types.
-}
module Triphut.Tally (
  -- * Datums
  TallyDynamicConfigDatum (..),

  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  TallyNftConfig (..),
  TallyValidatorConfig (..),
) where

import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.Prelude (BuiltinData, Integer)

-- | Tally policy configuration
data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  }

makeLift ''TallyNftConfig

-- | Tally config datum, representation mirrors the main 'Triphut.Types.DynamicConfigDatum'
data TallyDynamicConfigDatum = TallyDynamicConfigDatum
  { tdcTallyIndexNft :: BuiltinData
  , tdcTallyNft :: CurrencySymbol
  , tdcTallyValidator :: BuiltinData
  , tdcTreasuryValidator :: BuiltinData
  , tdcConfigurationValidator :: BuiltinData
  , tdcVoteCurrencySymbol :: CurrencySymbol
  , tdcVoteTokenName :: BuiltinData
  , tdcVoteValidator :: ValidatorHash
  , tdcUpgradeMajorityPercent :: BuiltinData
  , tdcUpgradRelativeMajorityPercent :: BuiltinData
  , tdcGeneralMajorityPercent :: BuiltinData
  , tdcGeneralRelativeMajorityPercent :: BuiltinData
  , tdcTripMajorityPercent :: BuiltinData
  , tdcTripRelativeMajorityPercent :: BuiltinData
  , tdcTotalVotes :: BuiltinData
  , tdcVoteNft :: CurrencySymbol
  , tdcVoteFungibleCurrencySymbol :: CurrencySymbol
  , tdcVoteFungibleTokenName :: TokenName
  , tdcProposalTallyEndOffset :: BuiltinData
  , tdcMaxGeneralDisbursement :: BuiltinData
  , tdcMaxTripDisbursement :: BuiltinData
  , tdcAgentDisbursementPercent :: BuiltinData
  , tdcFungibleVotePercent :: Integer
  }

unstableMakeIsData ''TallyDynamicConfigDatum

data TallyValidatorConfig = TallyValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

makeLift ''TallyValidatorConfig
