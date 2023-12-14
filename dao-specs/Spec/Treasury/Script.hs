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

import Dao.Treasury.Script (treasuryValidatorCompiledCode)
import Plutus.Model.V2 (TypedValidator, scriptHash)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')

-- Validator script and info
type TreasuryValidatorScript = TypedValidator () ()

treasuryTypedValidator :: TreasuryValidatorScript
treasuryTypedValidator = mkTypedValidator' treasuryValidatorCompiledCode sampleConfigValidatorConfig

treasuryValidatorScriptHash :: ScriptHash
treasuryValidatorScriptHash = scriptHash treasuryTypedValidator
