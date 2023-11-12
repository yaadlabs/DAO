module Triphut.Vote (
  VoteDatum (..),
  VoteDirection (..),
  VoteMinterConfig (..),
  VoteValidatorConfig (..),
  VoteDynamicConfig (..),
  VoteAction (..),
  VoteScriptContext (..),
  VoteTxOut (..),
  VoteTxInInfo (..),
  VoteTxInfo (..),
  VoteMinterTxOut (..),
  VoteMinterTxInfo (..),
  VoteMinterTxInInfo (..),
  VoteMinterDynamicConfigDatum (..),
  VoteMinterActionRedeemer (..),
  VoteMinterScriptContext (..),
  VoteMinterScriptPurpose (..),
  VoteAddress (..),
  VoteMinterAddress (..),
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Credential (Credential)
import Plutus.V1.Ledger.Scripts (
  Datum,
  DatumHash,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName,
  Value,
 )
import Plutus.V2.Ledger.Tx hiding (Mint)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Integer,
  (==),
 )
import PlutusTx.Prelude qualified as PlutusTx

data VoteMinterConfig = VoteMinterConfig
  { vmcConfigNftCurrencySymbol :: CurrencySymbol
  , vmcConfigNftTokenName :: TokenName
  }

makeLift ''VoteMinterConfig

data VoteDirection = For | Against

instance PlutusTx.Eq VoteDirection where
  For == For = True
  Against == Against = True
  _ == _ = False

data VoteDatum = VoteDatum
  { vProposalTokenName :: TokenName
  , vDirection :: VoteDirection
  , vOwner :: Address
  , vReturnAda :: Integer
  }

data VoteMinterActionRedeemer = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterActionRedeemer
unstableMakeIsData ''VoteDatum

data VoteMinterAddress = VoteMinterAddress
  { vmAddressCredential :: Credential
  , vmAddressStakingCredential :: BuiltinData
  }

data VoteMinterTxOut = VoteMinterTxOut
  { vmTxOutAddress :: VoteMinterAddress
  , vmTxOutValue :: Value
  , vmTxOutDatum :: OutputDatum
  , vmTxOutReferenceScript :: BuiltinData
  }

data VoteMinterTxInInfo = VoteMinterTxInInfo
  { vmTxInInfoOutRef :: BuiltinData
  , vmTxInInfoResolved :: VoteMinterTxOut
  }

newtype VoteMinterScriptPurpose = VMMinting CurrencySymbol

data VoteMinterScriptContext = VoteMinterScriptContext
  { vmScriptContextTxInfo :: VoteMinterTxInfo
  , vmScriptContextPurpose :: VoteMinterScriptPurpose
  }

data VoteMinterTxInfo = VoteMinterTxInfo
  { vmTxInfoInputs :: BuiltinData
  , vmTxInfoReferenceInputs :: BuiltinData
  , vmTxInfoOutputs :: BuiltinData
  , vmTxInfoFee :: BuiltinData
  , vmTxInfoMint :: Value
  , vmTxInfoDCert :: BuiltinData
  , vmTxInfoWdrl :: BuiltinData
  , vmTxInfoValidRange :: BuiltinData
  , vmTxInfoSignatories :: BuiltinData
  , vmTxInfoRedeemers :: BuiltinData
  , vmTxInfoData :: BuiltinData
  , vmTxInfoId :: BuiltinData
  }

data VoteMinterDynamicConfigDatum = VoteMinterDynamicConfigDatum
  { vmdcTallyIndexNft :: BuiltinData
  , vmdcTallyNft :: CurrencySymbol
  , vmdcTallyValidator :: BuiltinData
  , vmdcTreasuryValidator :: BuiltinData
  , vmdcConfigurationValidator :: BuiltinData
  , vmdcVoteCurrencySymbol :: BuiltinData
  , vmdcVoteTokenName :: TokenName
  , vmdcVoteValidator :: ValidatorHash
  , vmdcUpgradeMajorityPercent :: BuiltinData
  , vmdcUpgradRelativeMajorityPercent :: BuiltinData
  , vmdcGeneralMajorityPercent :: BuiltinData
  , vmdcGeneralRelativeMajorityPercent :: BuiltinData
  , vmdcTripMajorityPercent :: BuiltinData
  , vmdcTripRelativeMajorityPercent :: BuiltinData
  , vmdcTotalVotes :: BuiltinData
  , vmdcVoteNft :: CurrencySymbol
  , vmdcVoteFungibleCurrencySymbol :: BuiltinData
  , vmdcVoteFungibleTokenName :: BuiltinData
  , vmdcProposalTallyEndOffset :: BuiltinData
  , vmdcMaxGeneralDisbursement :: BuiltinData
  , vmdcMaxTripDisbursement :: BuiltinData
  , vmdcAgentDisbursementPercent :: BuiltinData
  , vmdcFungibleVotePercent :: BuiltinData
  }

unstableMakeIsData ''VoteMinterAddress
unstableMakeIsData ''VoteMinterTxOut
unstableMakeIsData ''VoteMinterTxInInfo
unstableMakeIsData ''VoteMinterScriptPurpose
unstableMakeIsData ''VoteMinterScriptContext
unstableMakeIsData ''VoteMinterTxInfo
unstableMakeIsData ''VoteMinterDynamicConfigDatum

data VoteAddress = VoteAddress
  { vAddressCredential :: Credential
  , vAddressStakingCredential :: BuiltinData
  }

data VoteTxOut = VoteTxOut
  { vTxOutAddress :: VoteAddress
  , vTxOutValue :: Value
  , vTxOutDatum :: OutputDatum
  , vTxOutReferenceScript :: BuiltinData
  }

data VoteTxInInfo = VoteTxInInfo
  { vTxInInfoOutRef :: TxOutRef
  , vTxInInfoResolved :: VoteTxOut
  }

data VoteScriptContext = VoteScriptContext
  { vScriptContextTxInfo :: VoteTxInfo
  , vScriptContextPurpose :: BuiltinData
  }

data VoteTxInfo = VoteTxInfo
  { vTxInfoInputs :: BuiltinData
  , vTxInfoReferenceInputs :: [VoteTxInInfo]
  , vTxInfoOutputs :: BuiltinData
  , vTxInfoFee :: BuiltinData
  , vTxInfoMint :: BuiltinData
  , vTxInfoDCert :: BuiltinData
  , vTxInfoWdrl :: BuiltinData
  , vTxInfoValidRange :: BuiltinData
  , vTxInfoSignatories :: BuiltinData
  , vTxInfoRedeemers :: BuiltinData
  , vTxInfoData :: Map DatumHash Datum
  , vTxInfoId :: BuiltinData
  }

data VoteDynamicConfig = VoteDynamicConfig
  { vdcTallyIndexNft :: BuiltinData
  , vdcTallyNft :: BuiltinData
  , vdcTallyValidator :: BuiltinData
  , vdcTreasuryValidator :: BuiltinData
  , vdcConfigurationValidator :: BuiltinData
  , vdcVoteCurrencySymbol :: BuiltinData
  , vdcVoteTokenName :: BuiltinData
  , vdcVoteValidator :: BuiltinData
  , vdcUpgradeMajorityPercent :: BuiltinData
  , vdcUpgradRelativeMajorityPercent :: BuiltinData
  , vdcGeneralMajorityPercent :: BuiltinData
  , vdcGeneralRelativeMajorityPercent :: BuiltinData
  , vdcTripMajorityPercent :: BuiltinData
  , vdcTripRelativeMajorityPercent :: BuiltinData
  , vdcTotalVotes :: BuiltinData
  , vdcVoteNft :: BuiltinData
  , vdcVoteFungibleCurrencySymbol :: BuiltinData
  , vdcVoteFungibleTokenName :: BuiltinData
  , vdcProposalTallyEndOffset :: BuiltinData
  , vdcMaxGeneralDisbursement :: BuiltinData
  , vdcMaxTripDisbursement :: BuiltinData
  , vdcAgentDisbursementPercent :: BuiltinData
  , vdcFungibleVotePercent :: BuiltinData
  }

data VoteAction
  = Count
  | Cancel

data VoteValidatorConfig = VoteValidatorConfig
  { vvcConfigNftCurrencySymbol :: CurrencySymbol
  , vvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''VoteAddress
unstableMakeIsData ''VoteTxOut
unstableMakeIsData ''VoteTxInInfo
unstableMakeIsData ''VoteScriptContext
unstableMakeIsData ''VoteTxInfo
unstableMakeIsData ''VoteAction
unstableMakeIsData ''VoteDynamicConfig
makeLift ''VoteValidatorConfig
