module Spec.ConfigurationNft.Script (
    mkValidNftTx
  , mkNftMinterTooManyTokensTx
) where

import Triphut.Types (DynamicConfig)
import Triphut.ConfigurationNft (ConfigurationValidatorConfig)
import Spec.ConfigurationNft.SampleData (sampleDynamicConfig, sampleConfigValidatorConfig)
import Plutus.Model.V2 (
  TypedValidator(TypedValidator),
  TypedPolicy,
  DatumMode(InlineDatum),
  toV2,
  payToScript,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, TokenName(TokenName), singleton)
import PlutusTx qualified
import Prelude (mconcat)
import PlutusTx.Prelude (($), (.), (<>))
import Triphut.ConfigurationNft (NftConfig(..))
import Triphut.ConfigurationNft.Script (mkNftMinter, configurationValidator)
import Plutus.Model 
  ( Tx
  , UserSpend
  , Run
  , Ada(Lovelace)
  , ada
  , newUser
  , adaValue
  , mintValue
  , userSpend
  , submitTx
  , getHeadRef
  , spend
  )

-- Policy script and info
configNftTypedMintingPolicy :: NftConfig -> TypedPolicy ()
configNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

nftCurrencySymbol :: NftConfig -> CurrencySymbol
nftCurrencySymbol = scriptCurrencySymbol . configNftTypedMintingPolicy

validNftConfigValue :: NftConfig -> Value
validNftConfigValue nftCfg@(NftConfig _ tokenName) = singleton (nftCurrencySymbol nftCfg) tokenName 1

invalidTooManyTokensConfigValue :: NftConfig -> Value
invalidTooManyTokensConfigValue nftCfg@(NftConfig _ tokenName) = singleton (nftCurrencySymbol nftCfg) tokenName 2

-- Validator script and info
type ConfigValidatorScript = TypedValidator DynamicConfig ()

configTypedValidator :: ConfigValidatorScript
configTypedValidator = mkTypedValidator' sampleConfigValidatorConfig

mkTypedValidator' :: ConfigurationValidatorConfig -> ConfigValidatorScript
mkTypedValidator' cfg = TypedValidator . toV2 $ configurationValidator cfg

-- Test txs

-- | A valid tx, test should pass
mkValidNftTx :: Run ()
mkValidNftTx = do
  user <- newUser $ ada (Lovelace 2_000_000)
  sp <- spend user (adaValue 2)
  let params = NftConfig (getHeadRef sp) (TokenName "triphut")
  submitTx user $ validNftTx params sp

validNftTx :: NftConfig -> UserSpend -> Tx
validNftTx config sp =
  let mintVal = validNftConfigValue config
      policy = configNftTypedMintingPolicy config
      validator = configTypedValidator
   in mconcat
       [ mintValue policy () mintVal
       , userSpend sp
       , payToScript validator (InlineDatum sampleDynamicConfig) (adaValue 2 <> mintVal)
       ]

-- | Invalid transaction as we mint more than one token, test should fail
mkNftMinterTooManyTokensTx :: Run ()
mkNftMinterTooManyTokensTx = do
  user <- newUser $ ada (Lovelace 2_000_000)
  sp <- spend user (adaValue 2)
  let params = NftConfig (getHeadRef sp) (TokenName "triphut")
  submitTx user $ invalidNftTooManyTokensTx params sp

invalidNftTooManyTokensTx :: NftConfig -> UserSpend -> Tx
invalidNftTooManyTokensTx config sp =
  let mintVal = invalidTooManyTokensConfigValue config
      policy = configNftTypedMintingPolicy config
      validator = configTypedValidator
   in mconcat
       [ mintValue policy () mintVal
       , userSpend sp
       , payToScript validator (InlineDatum sampleDynamicConfig) (adaValue 2 <> mintVal)
       ]
