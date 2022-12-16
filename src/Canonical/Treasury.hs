module Canonical.Treasury where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
-- import           Plutus.V1.Ledger.Address
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Credential
-- import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Tx hiding (Mint)
import           PlutusTx.AssocMap (Map)
-- import qualified PlutusTx.AssocMap as M
import           PlutusTx
import           PlutusTx.Prelude
import           Canonical.Shared
-- import           Canonical.Types
import qualified Plutonomy

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data TreasuryAddress = TreasuryAddress
  { tAddressCredential        :: Credential
  , tAddressStakingCredential :: BuiltinData
  }

data TreasuryTxOut = TreasuryTxOut
  { tTxOutAddress             :: TreasuryAddress
  , tTxOutValue               :: Value
  , tTxOutDatum               :: OutputDatum
  , tTxOutReferenceScript     :: BuiltinData
  }

data TreasuryTxInInfo = TreasuryTxInInfo
  { tTxInInfoOutRef   :: TxOutRef
  , tTxInInfoResolved :: TreasuryTxOut
  }

data TreasuryScriptPurpose = TreasurySpend TxOutRef

data TreasuryScriptContext = TreasuryScriptContext
  { tScriptContextTxInfo  :: TreasuryTxInfo
  , tScriptContextPurpose :: TreasuryScriptPurpose
  }

data TreasuryTxInfo = TreasuryTxInfo
  { tTxInfoInputs             :: [TreasuryTxInInfo]
  , tTxInfoReferenceInputs    :: [TreasuryTxInInfo]
  , tTxInfoOutputs            :: [TreasuryTxOut]
  , tTxInfoFee                :: BuiltinData
  , tTxInfoMint               :: BuiltinData
  , tTxInfoDCert              :: BuiltinData
  , tTxInfoWdrl               :: BuiltinData
  , tTxInfoValidRange         :: BuiltinData
  , tTxInfoSignatories        :: [PubKeyHash]
  , tTxInfoRedeemers          :: BuiltinData
  , tTxInfoData               :: Map DatumHash Datum
  , tTxInfoId                 :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data TreasuryAction
  = Count
  | Cancel

data Treasury = Treasury

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName      :: TokenName
  }

unstableMakeIsData ''TreasuryAddress
unstableMakeIsData ''TreasuryTxOut
unstableMakeIsData ''TreasuryTxInInfo
makeIsDataIndexed  ''TreasuryScriptPurpose [('TreasurySpend,1)]
unstableMakeIsData ''TreasuryScriptContext
unstableMakeIsData ''TreasuryTxInfo
unstableMakeIsData ''Treasury
unstableMakeIsData ''TreasuryAction
makeLift ''TreasuryValidatorConfig

-- Needs to work in bulk
validateTreasury
  :: TreasuryValidatorConfig
  -> Treasury
  -> TreasuryAction
  -> TreasuryScriptContext
  -> Bool
validateTreasury
  TreasuryValidatorConfig {}
  Treasury {}
  _action
  TreasuryScriptContext
    { tScriptContextTxInfo = TreasuryTxInfo {}
    } = error ()

wrapValidateTreasury
    :: TreasuryValidatorConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateTreasury cfg x y z = check (
  validateTreasury
    cfg
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

treasuryValidator :: TreasuryValidatorConfig -> Validator
treasuryValidator cfg = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay      = False
      , Plutonomy.ooFloatOutLambda  = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateTreasury ||])
    `applyCode`
    liftCode cfg

treasuryValidatorHash :: TreasuryValidatorConfig -> ValidatorHash
treasuryValidatorHash = validatorHash . treasuryValidator

treasuryScript :: TreasuryValidatorConfig ->  PlutusScript PlutusScriptV2
treasuryScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . treasuryValidator
