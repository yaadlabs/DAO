module Spec.Index.Script (
  indexConfigNftMintingPolicy,
  indexConfigNftTypedMintingPolicy,
  indexConfigNftValue,
  indexConfigNftCurrencySymbol,
)
where

import Plutus.Model.V2 (
  TypedPolicy,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  mkMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Triphut.Index (IndexNftConfig (IndexNftConfig))
import Triphut.Index.Script (mkIndexNftMinter)

indexConfigNftTypedMintingPolicy :: IndexNftConfig -> TypedPolicy ()
indexConfigNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkIndexNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

indexConfigNftMintingPolicy :: IndexNftConfig -> MintingPolicy
indexConfigNftMintingPolicy config =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkIndexNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

indexConfigNftCurrencySymbol :: IndexNftConfig -> CurrencySymbol
indexConfigNftCurrencySymbol = scriptCurrencySymbol . indexConfigNftTypedMintingPolicy

indexConfigNftValue :: IndexNftConfig -> Value
indexConfigNftValue nftCfg@(IndexNftConfig _ tokenName _) = singleton (indexConfigNftCurrencySymbol nftCfg) tokenName 1