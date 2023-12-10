{- |
Module      : Spec.Index.Script
Description : Index scripts
-}
module Spec.Index.Script (
  -- * Validator
  IndexValidatorScript,
  indexNftTypedValidator,
  indexValidatorScriptHash,

  -- * Minting policy
  indexConfigNftTypedMintingPolicy,
  indexConfigNftValue,
  indexConfigNftCurrencySymbol,
)
where

import Dao.Index.Script (indexValidatorCompiledCode, mkIndexNftMinter)
import Dao.ScriptArgument (IndexNftConfig (IndexNftConfig))
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum)
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
import PlutusTx.Prelude (const, id, ($), (.))
import Spec.SpecUtils (mkTypedValidator')

-- Policy script and info
indexConfigNftTypedMintingPolicy :: IndexNftConfig -> TypedPolicy ()
indexConfigNftTypedMintingPolicy config =
  mkTypedPolicy
    $ $$(PlutusTx.compile [||toBuiltinPolicy . mkIndexNftMinter||])
    `PlutusTx.applyCode` PlutusTx.liftCode config

indexConfigNftCurrencySymbol :: IndexNftConfig -> CurrencySymbol
indexConfigNftCurrencySymbol = scriptCurrencySymbol . indexConfigNftTypedMintingPolicy

indexConfigNftValue :: IndexNftConfig -> Value
indexConfigNftValue nftCfg@(IndexNftConfig _ tokenName _) =
  singleton (indexConfigNftCurrencySymbol nftCfg) tokenName 1

-- Validator script and info
type IndexValidatorScript = TypedValidator IndexNftDatum ()

indexNftTypedValidator :: IndexValidatorScript
indexNftTypedValidator = mkTypedValidator' (const indexValidatorCompiledCode) id

indexValidatorScriptHash :: ScriptHash
indexValidatorScriptHash = scriptHash indexNftTypedValidator
