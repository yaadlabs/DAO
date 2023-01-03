module Canonical.AlwaysSucceed where
import           Canonical.Shared
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude

succeedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
succeedValidator _ _ _ = True

succeedWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
succeedWrapped x y z = check
  (succeedValidator
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z)
  )

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| succeedWrapped ||])

succeed :: PlutusScript PlutusScriptV2
succeed = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator

succeedHash :: ValidatorHash
succeedHash = validatorHash validator

succeedValidator1 :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
succeedValidator1 _ _ ScriptContext {scriptContextTxInfo = TxInfo {..}} = txInfoId == txInfoId

succeedWrapped1 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
succeedWrapped1 x y z = check
  (succeedValidator1
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z)
  )

validator1 :: Validator
validator1 = mkValidatorScript $$(PlutusTx.compile [|| succeedWrapped1 ||])

succeed1 :: PlutusScript PlutusScriptV2
succeed1 = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator1

succeedHash1 :: ValidatorHash
succeedHash1 = validatorHash validator1
