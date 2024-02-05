{- |
Module      : Spec.Index.Script
Description : Index scripts
-}
module Spec.Index.Script (
  -- * Validator
  IndexValidatorScript,
  indexTypedValidator,
  indexValidatorScriptHash,

  -- * Minting policy
  indexTypedMintingPolicy,
  indexValue,
  indexCurrencySymbol,
)
where

import Dao.Index.Script (indexValidatorCompiledCode, mkIndexMinter)
import Dao.ScriptArgument (IndexPolicyParams (IndexPolicyParams))
import LambdaBuffers.ApplicationTypes.Index (IndexDatum)
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
indexTypedMintingPolicy :: IndexPolicyParams -> TypedPolicy ()
indexTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkIndexMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

indexCurrencySymbol :: IndexPolicyParams -> CurrencySymbol
indexCurrencySymbol = scriptCurrencySymbol . indexTypedMintingPolicy

indexValue :: IndexPolicyParams -> Value
indexValue nftCfg@(IndexPolicyParams _ tokenName _) =
  singleton (indexCurrencySymbol nftCfg) tokenName 1

-- Validator script and info
type IndexValidatorScript = TypedValidator IndexDatum ()

indexTypedValidator :: IndexValidatorScript
indexTypedValidator = mkTypedValidator' (const indexValidatorCompiledCode) id

indexValidatorScriptHash :: ScriptHash
indexValidatorScriptHash = scriptHash indexTypedValidator
