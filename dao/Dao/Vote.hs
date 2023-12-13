{- |
Module: Dao.Vote
Description: Contains all the voting specific types.
-}
module Dao.Vote (
  -- * Datums
  VoteDatum (..),
  VoteMinterDynamicConfigDatum (..),
  VoteDynamicConfigDatum (..),

  -- * Redeemers
  VoteMinterActionRedeemer (..),
  VoteActionRedeemer (..),

  -- * General vote related types
  VoteDirection (..),

  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  VoteMinterConfig (..),
  VoteValidatorConfig (..),
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Credential (Credential)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value)
import Plutus.V2.Ledger.Tx (OutputDatum, TxOutRef)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Integer,
  (==),
 )
import PlutusTx.Prelude qualified as PlutusTx

-- | 'Dao.Vote.Script.mkVoteMinter' argument
data VoteMinterConfig = VoteMinterConfig
  { vmcConfigNftCurrencySymbol :: CurrencySymbol
  , vmcConfigNftTokenName :: TokenName
  }

makeLift ''VoteMinterConfig

-- | Vote direction
data VoteDirection = For | Against

instance PlutusTx.Eq VoteDirection where
  For == For = True
  Against == Against = True
  _ == _ = False

-- | The vote datum, represnting a vote cast by a user on a specific proposal
data VoteDatum = VoteDatum
  { vProposalTokenName :: TokenName
  -- ^ The name of the proposal for which this vote relates to.
  -- This is checked in 'Dao.Tally.Script.validateTally' to ensure
  -- the vote is for the correct proposal.
  , vDirection :: VoteDirection
  -- ^ Whether the vote is for or against the proposal.
  , vOwner :: Address
  -- ^ The address of the user casting the vote.
  , vReturnAda :: Integer
  -- ^ Ada amount to return
  }

-- | Redeemer for 'Dao.Vote.Script.mkVoteMinter' policy
data VoteMinterActionRedeemer = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterActionRedeemer
unstableMakeIsData ''VoteDatum

data VoteMinterTxOut = VoteMinterTxOut
  { vmTxOutAddress :: VoteMinterAddress
  , vmTxOutValue :: Value
  , vmTxOutDatum :: OutputDatum
  , vmTxOutReferenceScript :: BuiltinData
  }

data VoteMinterAddress = VoteMinterAddress
  { vmAddressCredential :: Credential
  , vmAddressStakingCredential :: BuiltinData
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

-- | Vote minter config datum, representation mirrors the main 'Dao.Types.DynamicConfigDatum'
data VoteMinterDynamicConfigDatum = VoteMinterDynamicConfigDatum
  { vmdcTallyNft :: CurrencySymbol
  , vmdcVoteTokenName :: TokenName
  , vmdcVoteValidator :: ValidatorHash
  , vmdcVoteNft :: CurrencySymbol
  }

-- | Vote config datum, representation mirrors the main 'Dao.Types.DynamicConfigDatum'
data VoteDynamicConfigDatum = VoteDynamicConfigDatum
  { vdcTallyValidator :: ValidatorHash
  , vdcVoteCurrencySymbol :: BuiltinData
  }

unstableMakeIsData ''VoteMinterDynamicConfigDatum

-- | Redeemer for 'Dao.Vote.Script.validateVote' validator
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

-- | Redeemer for 'Dao.Vote.Script.validateVote' validator
data VoteActionRedeemer
  = Count
  | Cancel

data VoteValidatorConfig = VoteValidatorConfig
  { vvcConfigNftCurrencySymbol :: CurrencySymbol
  , vvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''VoteActionRedeemer
unstableMakeIsData ''VoteDynamicConfigDatum
makeLift ''VoteValidatorConfig
