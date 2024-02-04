{- |
Module      : Spec.Treasury.Script
Description : Treasury script
-}
module Spec.Treasury.Script (
  TreasuryValidatorScript,
  treasuryTypedValidator,
  treasuryValidatorScriptHash,
)
where

import Dao.ScriptArgument (ValidatorParams)
import Dao.Treasury.Script (validateTreasury)
import Plutus.Model.V2 (TypedValidator, mkTypedValidator, scriptHash, toBuiltinValidator)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData)
import Spec.Configuration.SampleData (sampleValidatorParams)
import Prelude ((.))

-- Validator script and info
type TreasuryValidatorScript = TypedValidator () ()

treasuryValidatorScriptHash :: ScriptHash
treasuryValidatorScriptHash = scriptHash treasuryTypedValidator

treasuryTypedValidator :: TreasuryValidatorScript
treasuryTypedValidator = treasuryTypedValidator' sampleValidatorParams

treasuryTypedValidator' :: ValidatorParams -> TreasuryValidatorScript
treasuryTypedValidator' config =
  mkTypedValidator
    (compiledTreasuryValidator `PlutusTx.applyCode` PlutusTx.liftCode config)

compiledTreasuryValidator ::
  PlutusTx.CompiledCode (ValidatorParams -> (BuiltinData -> BuiltinData -> BuiltinData -> ()))
compiledTreasuryValidator =
  $$(PlutusTx.compile [||toBuiltinValidator . validateTreasury||])
