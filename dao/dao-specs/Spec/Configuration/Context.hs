{- |
Module      : Spec.Configuration.Context
Description : Configuration policy unit tests
-}
module Spec.Configuration.Context (
  validConfigNftTest,
  invalidConfigNftTooManyTokensMintedTest,
  invalidConfigNftNoDatumPaidToScriptTest,
  invalidConfigNftWrongTokenNameTest,
) where

import Dao.ScriptArgument (ConfigPolicyParams (ConfigPolicyParams))
import Plutus.Model (
  Run,
  Tx,
  UserSpend,
  adaToken,
  adaValue,
  getHeadRef,
  logInfo,
  mintValue,
  newUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToKey,
  payToRef,
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (TokenName (TokenName), Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False, True), ($))
import Spec.Configuration.Script (
  configNftCurrencySymbol,
  configNftTypedMintingPolicy,
  upgradeConfigNftTypedValidator,
 )
import Spec.SampleData (sampleDynamicConfig)
import Spec.SpecUtils (minAda)
import Prelude (error, mconcat, show, (<>))

-- Test txs

-- | Valid test
validConfigNftTest :: Run ()
validConfigNftTest = mkConfigNftTest validNftTx

-- | Invalid test
invalidConfigNftTooManyTokensMintedTest :: Run ()
invalidConfigNftTooManyTokensMintedTest = mkConfigNftTest invalidNftTooManyTokensTx

-- | Invalid test
invalidConfigNftWrongTokenNameTest :: Run ()
invalidConfigNftWrongTokenNameTest = mkConfigNftTest invalidNftWrongTokenNameTokensTx

-- | Invalid test
invalidConfigNftNoDatumPaidToScriptTest :: Run ()
invalidConfigNftNoDatumPaidToScriptTest = mkConfigNftTest invalidNftNoDatumSentToValidator

-- | A valid tx, corresponding test should pass
validNftTx :: ConfigPolicyParams -> UserSpend -> PubKeyHash -> Tx
validNftTx = mkConfigNftTx True validConfigPolicyParamsValue

-- | Invalid tx that mints 2 tokens instead of 1, corresponding test should fail
invalidNftTooManyTokensTx :: ConfigPolicyParams -> UserSpend -> PubKeyHash -> Tx
invalidNftTooManyTokensTx = mkConfigNftTx True invalidTooManyTokensConfigValue

-- | Invalid tx that has the wrong token name, corresponding test should fail
invalidNftWrongTokenNameTokensTx :: ConfigPolicyParams -> UserSpend -> PubKeyHash -> Tx
invalidNftWrongTokenNameTokensTx = mkConfigNftTx True invalidWrongTokenNameConfigValue

{- | Invalid tx as we set the flag for paying the Datum to the
 validator script to False, corresponding test should fail
-}
invalidNftNoDatumSentToValidator :: ConfigPolicyParams -> UserSpend -> PubKeyHash -> Tx
invalidNftNoDatumSentToValidator = mkConfigNftTx False validConfigPolicyParamsValue

-- | Helper function for making tests
mkConfigNftTest :: (ConfigPolicyParams -> UserSpend -> PubKeyHash -> Tx) -> Run ()
mkConfigNftTest tx = do
  user <- newUser minAda
  spend' <- spend user (adaValue 2)
  let config = ConfigPolicyParams (getHeadRef spend') (TokenName "triphut")
  let configData = PlutusTx.toBuiltinData config
  let configData' = PlutusTx.toData config
  logInfo $ show configData'
  -- error $ show configData
  submitTx user $ tx config spend' user

{- | Helper function for building txs
 Set the `hasDatum` flag to False to create an invalid tx that
 doesn't pay the datum to the validator script
-}
mkConfigNftTx :: Bool -> (ConfigPolicyParams -> Value) -> ConfigPolicyParams -> UserSpend -> PubKeyHash -> Tx
mkConfigNftTx hasDatum configValue config spend' user =
  let
    -- Set up the value and scripts
    configValue' = configValue config
    configPolicy = configNftTypedMintingPolicy config

    -- Set up the txs
    baseTx = mconcat [mintValue configPolicy () configValue', userSpend spend']
    withDatum =
      payToRef
        upgradeConfigNftTypedValidator
        (InlineDatum sampleDynamicConfig)
        (adaValue 2 <> configValue')
    withNoDatumToUser = payToKey user (adaValue 2 <> configValue')
   in
    -- If hasDatum is set to False we want the withNoDatumToUser tx
    -- in order to trigger the negative test
    if hasDatum then baseTx <> withDatum else baseTx <> withNoDatumToUser

-- | Valid value to be used in valid tx
validConfigPolicyParamsValue :: ConfigPolicyParams -> Value
validConfigPolicyParamsValue nftCfg@(ConfigPolicyParams _ tokenName) =
  singleton (configNftCurrencySymbol nftCfg) tokenName 1

-- | Invalid value to be used in invalid tx
invalidTooManyTokensConfigValue :: ConfigPolicyParams -> Value
invalidTooManyTokensConfigValue nftCfg@(ConfigPolicyParams _ tokenName) =
  singleton (configNftCurrencySymbol nftCfg) tokenName 2

-- | Invalid value to be used in invalid tx
invalidWrongTokenNameConfigValue :: ConfigPolicyParams -> Value
invalidWrongTokenNameConfigValue nftCfg@(ConfigPolicyParams _ _) =
  singleton (configNftCurrencySymbol nftCfg) adaToken 1
