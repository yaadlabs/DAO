module Triphut.ConfigurationNft (
  ConfigurationValidatorConfig (..),
  ConfigurationTxInfo (..),
  ConfigurationTxInInfo (..),
  ConfigurationTxOut (..),
  ConfigurationScriptContext (..),
  ConfigurationScriptPurpose (..),
  NftConfig (..),
) where

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
  , cTxInfoMint :: Value
  , cTxInfoValidRange :: POSIXTimeRange
  , cTxInfoData :: Map DatumHash Datum
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
