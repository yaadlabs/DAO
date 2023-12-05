{- |
Module      : Spec.Index.Context
Description : Index policy unit tests
-}
module Spec.Index.Context where -- (
--   validIndexConfigNftTest,
--   invalidMoreThanOneTokenMintedIndexConfigNftTest,
--   invalidNoDatumSentToValidadtorIndexConfigNftTest,
-- ) where

import Dao.Index (IndexNftConfig (IndexNftConfig))
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
import Spec.Index.SampleData (validSampleIndexNftDatum)
import Spec.Index.Script (
  indexConfigNftCurrencySymbol,
  indexConfigNftTypedMintingPolicy,
  -- indexNftTypedValidator,
  -- indexValidatorHash',
 )
import Spec.SpecUtils (minAda)
import Spec.Values (dummyIndexConfigNftTokenName)
import Prelude (mconcat, (<>))

{- | Valid test
validIndexConfigNftTest :: Run ()
validIndexConfigNftTest = mkIndexConfigNftTest validIndexNftTx

invalidMoreThanOneTokenMintedIndexConfigNftTest :: Run ()
invalidMoreThanOneTokenMintedIndexConfigNftTest = mkIndexConfigNftTest invalidMoreThanOneTokenMintedIndexNftTx

invalidNoDatumSentToValidadtorIndexConfigNftTest :: Run ()
invalidNoDatumSentToValidadtorIndexConfigNftTest =
  mkIndexConfigNftTest invalidNftNoDatumSentToValidatorTx

-- | A valid tx, corresponding test should pass
validIndexNftTx :: IndexNftConfig -> UserSpend -> PubKeyHash -> Tx
validIndexNftTx = mkIndexConfigNftTx True validIndexConfigNftValue

invalidMoreThanOneTokenMintedIndexNftTx :: IndexNftConfig -> UserSpend -> PubKeyHash -> Tx
invalidMoreThanOneTokenMintedIndexNftTx =
  mkIndexConfigNftTx
    True
    invalidMoreThanOneTokenMintedIndexConfigNftValue

invalidNftNoDatumSentToValidatorTx :: IndexNftConfig -> UserSpend -> PubKeyHash -> Tx
invalidNftNoDatumSentToValidatorTx = mkIndexConfigNftTx False validIndexConfigNftValue
-}

{- | Helper function for making tests
mkIndexConfigNftTest :: (IndexNftConfig -> UserSpend -> PubKeyHash -> Tx) -> Run ()
mkIndexConfigNftTest tx = do
  user <- newUser minAda
  spend' <- spend user (adaValue 2)
  let config = IndexNftConfig (getHeadRef spend') dummyIndexConfigNftTokenName indexValidatorHash'
  submitTx user $ tx config spend' user
-}

{- | Helper function for building txs
 Set the `hasDatum` flag to False to create an invalid tx that
 doesn't pay the datum to the validator script
-}

-- mkIndexConfigNftTx :: Bool -> (IndexNftConfig -> Value) -> IndexNftConfig -> UserSpend -> PubKeyHash -> Tx
-- mkIndexConfigNftTx hasDatum configValue config spend' user =
--   let
--     -- Set up the value and scripts
--     mintVal = configValue config
--     policy = indexConfigNftTypedMintingPolicy config
--     validator = indexNftTypedValidator
--
--     -- Set up the txs
--     baseTx = mconcat [mintValue policy () mintVal, userSpend spend']
--     withDatum = payToScript validator (InlineDatum validSampleIndexNftDatum) (adaValue 2 <> mintVal)
--     withNoDatumToUser = payToKey user (adaValue 2 <> mintVal)
--    in
--     -- If hasDatum is set to False we want the withNoDatumToUser tx
--     -- in order to trigger the negative test
--     if hasDatum then baseTx <> withDatum else baseTx <> withNoDatumToUser

{- | Valid value to be used in valid tx
validIndexConfigNftValue :: IndexNftConfig -> Value
validIndexConfigNftValue nftCfg@(IndexNftConfig _ tokenName _) =
  singleton (indexConfigNftCurrencySymbol nftCfg) tokenName 1

invalidMoreThanOneTokenMintedIndexConfigNftValue :: IndexNftConfig -> Value
invalidMoreThanOneTokenMintedIndexConfigNftValue nftCfg@(IndexNftConfig _ tokenName _) =
  singleton (indexConfigNftCurrencySymbol nftCfg) tokenName 2
-}
