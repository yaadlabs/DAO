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
import LambdaBuffers.ApplicationTypes.Treasury (TreasuryDatum)
import Plutus.Model.V2 (TypedValidator, mkTypedValidator, scriptHash)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinData)
import Spec.Configuration.SampleData (sampleValidatorParams)
import Spec.SpecUtils (mkTypedValidatorOptimized, mkUntypedValidator)
import Prelude ((.))

-- Validator script and info
type TreasuryValidatorScript = TypedValidator TreasuryDatum ()

treasuryValidatorScriptHash :: ScriptHash
treasuryValidatorScriptHash = scriptHash treasuryTypedValidator

treasuryTypedValidator :: TreasuryValidatorScript
treasuryTypedValidator = treasuryTypedValidator' sampleValidatorParams

treasuryTypedValidator' :: ValidatorParams -> TreasuryValidatorScript
treasuryTypedValidator' =
  mkTypedValidatorOptimized
    (\config -> compiledTreasuryValidator `PlutusTx.applyCode` PlutusTx.liftCode config)

compiledTreasuryValidator ::
  PlutusTx.CompiledCode (ValidatorParams -> (BuiltinData -> BuiltinData -> BuiltinData -> ()))
compiledTreasuryValidator =
  $$(PlutusTx.compile [||mkUntypedValidator . validateTreasury||])
