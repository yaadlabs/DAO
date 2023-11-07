{- |
Module      : Spec.ConfigurationNft.Context
Description : ConfigurationNft context unit tests
-}
module Spec.ConfigurationNft.Context (
  validConfigNftTest,
  invalidConfigNftTooManyTokensMintedTest,
  invalidConfigNftNoDatumPaidToScriptTest,
) where

import Plutus.Model (
  Ada (Lovelace),
  Run,
  Tx,
  UserSpend,
  ada,
  adaValue,
  getHeadRef,
  mintValue,
  newUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToScript,
 )
import Plutus.V1.Ledger.Value (TokenName (TokenName), Value, singleton)
import PlutusTx.Prelude (Bool (False, True), ($))
import Spec.ConfigurationNft.SampleData (sampleDynamicConfig)
import Spec.ConfigurationNft.Script (
  configNftCurrencySymbol,
  configNftTypedMintingPolicy,
  configNftTypedValidator,
 )
import Triphut.ConfigurationNft (NftConfig (NftConfig))
import Prelude (mconcat, (<>))

-- Test txs

-- | Valid test
validConfigNftTest :: Run ()
validConfigNftTest = mkConfigNftTest validNftTx

-- | Invalid test
invalidConfigNftTooManyTokensMintedTest :: Run ()
invalidConfigNftTooManyTokensMintedTest = mkConfigNftTest invalidNftTooManyTokensTx

-- | Invalid test
invalidConfigNftNoDatumPaidToScriptTest :: Run ()
invalidConfigNftNoDatumPaidToScriptTest = mkConfigNftTest invalidNftNoDatumSentToValidator

-- | A valid tx, corresponding test should pass
validNftTx :: NftConfig -> UserSpend -> Tx
validNftTx = mkConfigNftTx True validNftConfigValue

-- | Invalid tx that mints 2 tokens instead of 1, corresponding test should fail
invalidNftTooManyTokensTx :: NftConfig -> UserSpend -> Tx
invalidNftTooManyTokensTx = mkConfigNftTx True invalidTooManyTokensConfigValue

{- | Invalid tx as we set the flag for paying the Datum to the
 validator script to False, corresponding test should fail
-}
invalidNftNoDatumSentToValidator :: NftConfig -> UserSpend -> Tx
invalidNftNoDatumSentToValidator = mkConfigNftTx False validNftConfigValue

-- | Helper function for making tests
mkConfigNftTest :: (NftConfig -> UserSpend -> Tx) -> Run ()
mkConfigNftTest tx = do
  user <- newUser $ ada (Lovelace 2_000_000)
  spend' <- spend user (adaValue 2)
  let params = NftConfig (getHeadRef spend') (TokenName "triphut")
  submitTx user $ tx params spend'

{- | Helper function for building txs
 Set the `hasDatum` flag to False to create an invalid tx that
 doesn't pay the datum to the validator script
-}
mkConfigNftTx :: Bool -> (NftConfig -> Value) -> NftConfig -> UserSpend -> Tx
mkConfigNftTx hasDatum configValue config spend' =
  let mintVal = configValue config
      policy = configNftTypedMintingPolicy config
      validator = configNftTypedValidator
      baseTx = mconcat [mintValue policy () mintVal, userSpend spend']
      withDatum = payToScript validator (InlineDatum sampleDynamicConfig) (adaValue 2 <> mintVal)
   in if hasDatum then baseTx <> withDatum else baseTx

-- | Valid value to be used in valid tx
validNftConfigValue :: NftConfig -> Value
validNftConfigValue nftCfg@(NftConfig _ tokenName) =
  singleton (configNftCurrencySymbol nftCfg) tokenName 1

-- | Invalid value to be used in invalid tx
invalidTooManyTokensConfigValue :: NftConfig -> Value
invalidTooManyTokensConfigValue nftCfg@(NftConfig _ tokenName) =
  singleton (configNftCurrencySymbol nftCfg) tokenName 2
