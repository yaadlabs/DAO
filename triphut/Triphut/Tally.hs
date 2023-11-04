module Triphut.Tally (
  IndexValidatorConfig (..),
  IndexNftConfig (..),
  TallyNftConfig (..),
  TallyValidatorConfig (..),
  IndexNftDatum (..),
  TallyTxOut (..),
  TallyTxInInfo (..),
  TallyTxInfo (..),
  TallyScriptContext (..),
  TallyDynamicConfig (..),
  TallyScriptPurpose (..),
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Scripts (
  Datum,
  DatumHash,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value as V
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
import PlutusTx.Prelude (Integer)

newtype IndexNftDatum = IndexNftDatum
  { indIndex :: Integer
  }

data IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TxOutRef
  , incTokenName :: TokenName
  , incIndexValidator :: ValidatorHash
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig

-------------------------------------------------------------------------------
-- Nft Index Validator
-------------------------------------------------------------------------------

data IndexValidatorConfig = IndexValidatorConfig
  { ivcConfigNftCurrencySymbol :: CurrencySymbol
  , ivcConfigNftTokenName :: TokenName
  , ivcNonce :: Integer -- to help with testing
  }

makeLift ''IndexValidatorConfig

-------------------------------------------------------------------------------
-- Tally Nft Minter
-------------------------------------------------------------------------------

data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  }

makeLift ''TallyNftConfig

-------------------------------------------------------------------------------
-- Tally Validator
-------------------------------------------------------------------------------

data TallyTxOut = TallyTxOut
  { tTxOutAddress :: Address
  , tTxOutValue :: Value
  , tTxOutDatum :: OutputDatum
  }

data TallyTxInInfo = TallyTxInInfo
  { tTxInInfoOutRef :: TxOutRef
  , tTxInInfoResolved :: TallyTxOut
  }

newtype TallyScriptPurpose = TallySpend TxOutRef

data TallyScriptContext = TallyScriptContext
  { tScriptContextTxInfo :: TallyTxInfo
  , tScriptContextPurpose :: TallyScriptPurpose
  }

data TallyTxInfo = TallyTxInfo
  { tTxInfoInputs :: [TallyTxInInfo]
  , tTxInfoReferenceInputs :: [TallyTxInInfo]
  , tTxInfoOutputs :: [TallyTxOut]
  , tTxInfoValidRange :: POSIXTimeRange
  , tTxInfoData :: Map DatumHash Datum
  }

data TallyDynamicConfig = TallyDynamicConfig
  { tdcTallyNft :: CurrencySymbol
  , tdcVoteValidator :: ValidatorHash
  , tdcVoteNft :: CurrencySymbol
  , tdcVoteFungibleCurrencySymbol :: CurrencySymbol
  , tdcVoteFungibleTokenName :: TokenName
  , tdcFungibleVotePercent :: Integer
  }

unstableMakeIsData ''TallyDynamicConfig

data TallyValidatorConfig = TallyValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''TallyTxOut
unstableMakeIsData ''TallyTxInInfo
makeIsDataIndexed ''TallyScriptPurpose [('TallySpend, 1)]
unstableMakeIsData ''TallyScriptContext
unstableMakeIsData ''TallyTxInfo
makeLift ''TallyValidatorConfig
