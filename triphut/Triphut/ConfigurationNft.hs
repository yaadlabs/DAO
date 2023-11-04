module Triphut.ConfigurationNft (
  ConfigurationValidatorConfig (..),
  ConfigurationTxInfo (..),
  ConfigurationTxInInfo (..),
  ConfigurationTxOut (..),
  ConfigurationScriptContext (..),
  ConfigurationScriptPurpose (..),
  NftConfig (..),
) where

import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (
  Datum,
  DatumHash,
 )
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName,
  Value,
 )
import Plutus.V2.Ledger.Tx (
  OutputDatum,
  TxOutRef,
 )
import PlutusTx (
  makeIsDataIndexed,
  makeLift,
  unstableMakeIsData,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (BuiltinData)

data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  , ncTokenName :: TokenName
  }

makeLift ''NftConfig

data ConfigurationTxOut = ConfigurationTxOut
  { cTxOutValue :: Value
  , cTxOutDatum :: OutputDatum
  , cTxOutReferenceScript :: BuiltinData
  }

data ConfigurationTxInInfo = ConfigurationTxInInfo
  { cTxInInfoOutRef :: TxOutRef
  , cTxInInfoResolved :: ConfigurationTxOut
  }

newtype ConfigurationScriptPurpose = ConfigurationSpend TxOutRef

data ConfigurationScriptContext = ConfigurationScriptContext
  { cScriptContextTxInfo :: ConfigurationTxInfo
  , cScriptContextPurpose :: ConfigurationScriptPurpose
  }

data ConfigurationTxInfo = ConfigurationTxInfo
  { cTxInfoInputs :: [ConfigurationTxInInfo]
  , cTxInfoReferenceInputs :: [ConfigurationTxInInfo]
  , cTxInfoOutputs :: [ConfigurationTxOut]
  , cTxInfoFee :: BuiltinData
  , cTxInfoMint :: Value
  , cTxInfoDCert :: BuiltinData
  , cTxInfoWdrl :: BuiltinData
  , cTxInfoValidRange :: POSIXTimeRange
  , cTxInfoSignatories :: [PubKeyHash]
  , cTxInfoRedeemers :: BuiltinData
  , cTxInfoData :: Map DatumHash Datum
  , cTxInfoId :: BuiltinData
  }

data ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''ConfigurationTxOut
unstableMakeIsData ''ConfigurationTxInInfo
makeIsDataIndexed ''ConfigurationScriptPurpose [('ConfigurationSpend, 1)]
unstableMakeIsData ''ConfigurationScriptContext
unstableMakeIsData ''ConfigurationTxInfo
makeLift ''ConfigurationValidatorConfig
