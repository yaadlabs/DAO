{- |
Module      : Spec.Index.Script
Description : Index scripts
-}
module Spec.Index.Script (
  indexConfigNftTypedMintingPolicy,
  indexNftTypedValidator,
  indexConfigNftValue,
  indexConfigNftCurrencySymbol,
  indexValidatorHash',
)
where

import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.Index.SampleData (sampleIndexValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')
import Triphut.Index (IndexNftConfig (IndexNftConfig), IndexNftDatum)
import Triphut.Index.Script (indexValidator, indexValidatorHash, mkIndexNftMinter)

-- Policy script and info
indexConfigNftTypedMintingPolicy :: IndexNftConfig -> TypedPolicy ()
indexConfigNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkIndexNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

indexConfigNftCurrencySymbol :: IndexNftConfig -> CurrencySymbol
indexConfigNftCurrencySymbol = scriptCurrencySymbol . indexConfigNftTypedMintingPolicy

indexConfigNftValue :: IndexNftConfig -> Value
indexConfigNftValue nftCfg@(IndexNftConfig _ tokenName _) =
  singleton (indexConfigNftCurrencySymbol nftCfg) tokenName 1

-- Validator script and info
type IndexValidatorScript = TypedValidator IndexNftDatum ()

indexNftTypedValidator :: IndexValidatorScript
indexNftTypedValidator = mkTypedValidator' sampleIndexValidatorConfig indexValidator

indexValidatorHash' :: ValidatorHash
indexValidatorHash' = indexValidatorHash sampleIndexValidatorConfig
