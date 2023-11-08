module Triphut.Treasury (
  TreasuryValidatorConfig (..),
  TreasuryTxInfo (..),
  TreasuryTxInInfo (..),
  TreasuryTxOut (..),
  TreasuryScriptContext (..),
  TreasuryScriptPurpose (..),
  Treasury,
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash)
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value as V
import Plutus.V2.Ledger.Tx hiding (Mint)
import PlutusTx (
  makeIsDataIndexed,
  makeLift,
  unstableMakeIsData,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.Prelude (BuiltinData)

data TreasuryTxOut = TreasuryTxOut
  { tTxOutAddress :: Address
  , tTxOutValue :: Value
  , tTxOutDatum :: OutputDatum
  , tTxOutReferenceScript :: BuiltinData
  }

data TreasuryTxInInfo = TreasuryTxInInfo
  { tTxInInfoOutRef :: TxOutRef
  , tTxInInfoResolved :: TreasuryTxOut
  }

newtype TreasuryScriptPurpose = TreasurySpend TxOutRef

data TreasuryScriptContext = TreasuryScriptContext
  { tScriptContextTxInfo :: TreasuryTxInfo
  , tScriptContextPurpose :: TreasuryScriptPurpose
  }

data TreasuryTxInfo = TreasuryTxInfo
  { tTxInfoInputs :: [TreasuryTxInInfo]
  , tTxInfoReferenceInputs :: [TreasuryTxInInfo]
  , tTxInfoOutputs :: [TreasuryTxOut]
  , tTxInfoFee :: BuiltinData
  , tTxInfoMint :: Value
  , tTxInfoDCert :: BuiltinData
  , tTxInfoWdrl :: BuiltinData
  , tTxInfoValidRange :: POSIXTimeRange
  , tTxInfoSignatories :: [PubKeyHash]
  , tTxInfoRedeemers :: BuiltinData
  , tTxInfoData :: Map DatumHash Datum
  , tTxInfoId :: BuiltinData
  }

type Treasury = BuiltinData

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''TreasuryTxOut
unstableMakeIsData ''TreasuryTxInInfo
makeIsDataIndexed ''TreasuryScriptPurpose [('TreasurySpend, 1)]
unstableMakeIsData ''TreasuryScriptContext
unstableMakeIsData ''TreasuryTxInfo
makeLift ''TreasuryValidatorConfig
