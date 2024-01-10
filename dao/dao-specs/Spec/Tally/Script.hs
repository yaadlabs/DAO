{- |
Module      : Spec.Tally.Script
Description : Tally scripts
-}
module Spec.Tally.Script (
  -- * Minting policy
  tallyConfigNftTypedMintingPolicy,
  tallyConfigNftValue,
  tallyConfigNftCurrencySymbol,

  -- * Validator
  tallyNftTypedValidator,
  tallyValidatorScriptHash,
)
where

import Dao.ScriptArgument (TallyNftConfig (TallyNftConfig))
import Dao.Tally.Script (mkTallyNftMinter, tallyValidatorCompiledCode)
import LambdaBuffers.ApplicationTypes.Tally (TallyStateDatum)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
  scriptHash,
  toBuiltinPolicy,
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.Configuration.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')

-- Policy script and info
tallyConfigNftTypedMintingPolicy :: TallyNftConfig -> TypedPolicy ()
tallyConfigNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkTallyNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

tallyConfigNftCurrencySymbol :: TallyNftConfig -> CurrencySymbol
tallyConfigNftCurrencySymbol = scriptCurrencySymbol . tallyConfigNftTypedMintingPolicy

tallyConfigNftValue :: TallyNftConfig -> Value
tallyConfigNftValue nftCfg@(TallyNftConfig _ tokenName _ _) =
  singleton (tallyConfigNftCurrencySymbol nftCfg) tokenName 1

-- Validator script and info
type TallyValidatorScript = TypedValidator TallyStateDatum ()

tallyNftTypedValidator :: TallyValidatorScript
tallyNftTypedValidator = mkTypedValidator' tallyValidatorCompiledCode sampleConfigValidatorConfig

tallyValidatorScriptHash :: ScriptHash
tallyValidatorScriptHash = scriptHash tallyNftTypedValidator
