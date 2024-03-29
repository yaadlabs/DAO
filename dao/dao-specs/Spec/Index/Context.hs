{- |
Module      : Spec.Index.Context
Description : Index policy unit tests
-}
module Spec.Index.Context (
  validIndexConfigNftTest,
  invalidMoreThanOneTokenMintedIndexConfigNftTest,
  invalidNoDatumSentToValidadtorIndexConfigNftTest,
) where

import Dao.ScriptArgument (IndexPolicyParams (IndexPolicyParams))
import Plutus.Model (
  Run,
  Tx,
  UserSpend,
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
  payToKey,
  payToScript,
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (Value, singleton)
import PlutusTx.Prelude (Bool (False, True), ($))
import Spec.Index.SampleData (validSampleIndexDatum)
import Spec.Index.Script (
  indexCurrencySymbol,
  indexTypedMintingPolicy,
  indexTypedValidator,
  indexValidatorScriptHash,
 )
import Spec.SpecUtils (minAda)
import Spec.Values (dummyIndexConfigNftTokenName)
import Prelude (mconcat, (<>))

-- | Valid test
validIndexConfigNftTest :: Run ()
validIndexConfigNftTest = mkIndexConfigNftTest validIndexNftTx

invalidMoreThanOneTokenMintedIndexConfigNftTest :: Run ()
invalidMoreThanOneTokenMintedIndexConfigNftTest = mkIndexConfigNftTest invalidMoreThanOneTokenMintedIndexNftTx

invalidNoDatumSentToValidadtorIndexConfigNftTest :: Run ()
invalidNoDatumSentToValidadtorIndexConfigNftTest =
  mkIndexConfigNftTest invalidNftNoDatumSentToValidatorTx

-- | A valid tx, corresponding test should pass
validIndexNftTx :: IndexPolicyParams -> UserSpend -> PubKeyHash -> Tx
validIndexNftTx = mkIndexConfigNftTx True validIndexConfigNftValue

invalidMoreThanOneTokenMintedIndexNftTx :: IndexPolicyParams -> UserSpend -> PubKeyHash -> Tx
invalidMoreThanOneTokenMintedIndexNftTx =
  mkIndexConfigNftTx
    True
    invalidMoreThanOneTokenMintedIndexConfigNftValue

invalidNftNoDatumSentToValidatorTx :: IndexPolicyParams -> UserSpend -> PubKeyHash -> Tx
invalidNftNoDatumSentToValidatorTx = mkIndexConfigNftTx False validIndexConfigNftValue

-- | Helper function for making tests
mkIndexConfigNftTest :: (IndexPolicyParams -> UserSpend -> PubKeyHash -> Tx) -> Run ()
mkIndexConfigNftTest tx = do
  user <- newUser minAda
  spend' <- spend user (adaValue 2)
  let config = IndexPolicyParams (getHeadRef spend') dummyIndexConfigNftTokenName indexValidatorScriptHash
  submitTx user $ tx config spend' user

{- | Helper function for building txs
 Set the `hasDatum` flag to False to create an invalid tx that
 doesn't pay the datum to the validator script
-}
mkIndexConfigNftTx :: Bool -> (IndexPolicyParams -> Value) -> IndexPolicyParams -> UserSpend -> PubKeyHash -> Tx
mkIndexConfigNftTx hasDatum configValue config spend' user =
  let
    -- Set up the value and scripts
    mintVal = configValue config
    policy = indexTypedMintingPolicy config
    validator = indexTypedValidator

    -- Set up the txs
    baseTx = mconcat [mintValue policy () mintVal, userSpend spend']
    withDatum = payToScript validator (InlineDatum validSampleIndexDatum) (adaValue 2 <> mintVal)
    withNoDatumToUser = payToKey user (adaValue 2 <> mintVal)
   in
    -- If hasDatum is set to False we want the withNoDatumToUser tx
    -- in order to trigger the negative test
    if hasDatum then baseTx <> withDatum else baseTx <> withNoDatumToUser

-- | Valid value to be used in valid tx
validIndexConfigNftValue :: IndexPolicyParams -> Value
validIndexConfigNftValue nftCfg@(IndexPolicyParams _ tokenName _) =
  singleton (indexCurrencySymbol nftCfg) tokenName 1

invalidMoreThanOneTokenMintedIndexConfigNftValue :: IndexPolicyParams -> Value
invalidMoreThanOneTokenMintedIndexConfigNftValue nftCfg@(IndexPolicyParams _ tokenName _) =
  singleton (indexCurrencySymbol nftCfg) tokenName 2
