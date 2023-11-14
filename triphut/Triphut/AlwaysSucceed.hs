{- |
Module: Triphut.AlwaysSucceed
Description: Contains all the always succeed script to be used for referencing.
-}
module Triphut.AlwaysSucceed (
  succeed,
  succeed1,
  succeedHash,
  succeedHash1,
  succeedValidator,
  succeedValidator1,
) where

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash, mkValidatorScript)
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInfo (TxInfo, txInfoId),
 )
import PlutusTx (compile, unsafeFromBuiltinData)
import PlutusTx.Prelude (Bool (True), BuiltinData, check, ($), (.), (==))
import Triphut.Shared (validatorHash)

succeedValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
succeedValidator _ _ _ = True

succeedWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
succeedWrapped x y z =
  check
    ( succeedValidator
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [||succeedWrapped||])

succeed :: PlutusScript PlutusScriptV2
succeed = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator

succeedHash :: ValidatorHash
succeedHash = validatorHash validator

succeedValidator1 :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
succeedValidator1 _ _ ScriptContext {scriptContextTxInfo = TxInfo {..}} = txInfoId == txInfoId

succeedWrapped1 :: BuiltinData -> BuiltinData -> BuiltinData -> ()
succeedWrapped1 x y z =
  check
    ( succeedValidator1
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

validator1 :: Validator
validator1 = mkValidatorScript $$(PlutusTx.compile [||succeedWrapped1||])

succeed1 :: PlutusScript PlutusScriptV2
succeed1 = PlutusScriptSerialised . SBS.toShort . LB.toStrict . serialise $ validator1

succeedHash1 :: ValidatorHash
succeedHash1 = validatorHash validator1
