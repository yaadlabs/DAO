{- |
Module      : Spec.Index.Script
Description : Index scripts
-}
module Spec.Index.Script (
  IndexValidatorScript,
  indexConfigNftTypedMintingPolicy,
  indexNftTypedValidator,
  indexConfigNftValue,
  indexConfigNftCurrencySymbol,
  indexValidatorHash',
)
where

import Dao.Index (IndexNftConfig (IndexNftConfig), IndexNftDatum)
import Dao.Index.Script (indexValidator, indexValidatorHash, mkIndexNftMinter)
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
indexNftTypedValidator = mkTypedValidator' id (const indexValidator)

indexValidatorHash' :: ValidatorHash
indexValidatorHash' = indexValidatorHash
