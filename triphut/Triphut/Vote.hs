module Triphut.Vote (
  Vote (..),
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
  VoteMinterDynamicConfig (..),
  VoteMinterAction (..),
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

data Vote = Vote
  { vProposalTokenName :: TokenName
  , vDirection :: VoteDirection
  , vOwner :: Address
  , vReturnAda :: Integer
  }

data VoteMinterAction = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterAction
unstableMakeIsData ''Vote

newtype VoteMinterAddress = VoteMinterAddress
  { vmAddressCredential :: Credential
  }

data VoteMinterTxOut = VoteMinterTxOut
  { vmTxOutAddress :: VoteMinterAddress
  , vmTxOutValue :: Value
  , vmTxOutDatum :: OutputDatum
  }

newtype VoteMinterTxInInfo = VoteMinterTxInInfo
  { vmTxInInfoResolved :: VoteMinterTxOut
  }

newtype VoteMinterScriptPurpose = VMMinting CurrencySymbol

data VoteMinterScriptContext = VoteMinterScriptContext
  { vmScriptContextTxInfo :: VoteMinterTxInfo
  , vmScriptContextPurpose :: VoteMinterScriptPurpose
  }

data VoteMinterTxInfo = VoteMinterTxInfo
  { vmTxInfoReferenceInputs :: BuiltinData
  , vmTxInfoOutputs :: BuiltinData
  , vmTxInfoMint :: Value
  , vmTxInfoValidRange :: BuiltinData
  , vmTxInfoData :: BuiltinData
  }

data VoteMinterDynamicConfig = VoteMinterDynamicConfig
  { vmdcTallyNft :: CurrencySymbol
  , vmdcVoteTokenName :: TokenName
  , vmdcVoteValidator :: ValidatorHash
  , vmdcVoteNft :: CurrencySymbol
  }

unstableMakeIsData ''VoteMinterAddress
unstableMakeIsData ''VoteMinterTxOut
unstableMakeIsData ''VoteMinterTxInInfo
unstableMakeIsData ''VoteMinterScriptPurpose
unstableMakeIsData ''VoteMinterScriptContext
unstableMakeIsData ''VoteMinterTxInfo
unstableMakeIsData ''VoteMinterDynamicConfig

data VoteAddress = VoteAddress
  { vAddressCredential :: Credential
  , vAddressStakingCredential :: BuiltinData
  }

data VoteTxOut = VoteTxOut
  { vTxOutAddress :: VoteAddress
  , vTxOutValue :: Value
  , vTxOutDatum :: OutputDatum
  }

newtype VoteTxInInfo = VoteTxInInfo
  { vTxInInfoResolved :: VoteTxOut
  }

newtype VoteScriptContext = VoteScriptContext
  { vScriptContextTxInfo :: VoteTxInfo
  }

data VoteTxInfo = VoteTxInfo
  { vTxInfoInputs :: BuiltinData
  , vTxInfoReferenceInputs :: [VoteTxInInfo]
  , vTxInfoOutputs :: BuiltinData
  , vTxInfoSignatories :: BuiltinData
  , vTxInfoData :: Map DatumHash Datum
  }

data VoteDynamicConfig = VoteDynamicConfig
  { vdcTallyValidator :: BuiltinData
  , vdcVoteCurrencySymbol :: BuiltinData
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
