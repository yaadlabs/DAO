module Spec.ConfigurationNft.Script (
  configNftTypedMintingPolicy,
  configNftMintingPolicy,
  nftConfigValue,
) where

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
import Triphut.ConfigurationNft (NftConfig (NftConfig), mkNftMinter)

configNftTypedMintingPolicy :: NftConfig -> TypedPolicy ()
configNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

configNftMintingPolicy :: NftConfig -> MintingPolicy
configNftMintingPolicy config =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

nftCurrencySymbol :: NftConfig -> CurrencySymbol
nftCurrencySymbol = scriptCurrencySymbol . configNftTypedMintingPolicy

nftConfigValue :: NftConfig -> Value
nftConfigValue nftCfg@(NftConfig _ tokenName) = singleton (nftCurrencySymbol nftCfg) tokenName 1
