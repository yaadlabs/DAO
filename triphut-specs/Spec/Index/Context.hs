{- |
Module      : Spec.Index.Context
Description : Index policy context unit tests
-}
module Spec.Index.Context (validIndexConfigNftTest) where

import Plutus.Model (
  Ada (Lovelace),
  Run,
  Tx,
  UserSpend,
  ada,
  adaToken,
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
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (TokenName), Value, singleton)
import PlutusTx.Prelude (Bool (True), ($))
import Spec.Index.SampleData (validSampleIndexNftDatum)
import Spec.Index.Script (
  indexConfigNftCurrencySymbol,
  indexConfigNftTypedMintingPolicy,
  indexNftTypedValidator,
  indexValidatorHash',
 )
import Triphut.Index (IndexNftConfig (IndexNftConfig))
import Prelude (mconcat, (<>))

-- | Valid test
validIndexConfigNftTest :: Run ()
validIndexConfigNftTest = mkIndexConfigNftTest validIndexNftTx

-- | Helper function for making tests
mkIndexConfigNftTest :: (IndexNftConfig -> UserSpend -> PubKeyHash -> Tx) -> Run ()
mkIndexConfigNftTest tx = do
  user <- newUser $ ada (Lovelace 2_000_000)
  spend' <- spend user (adaValue 2)
  let config = IndexNftConfig (getHeadRef spend') (TokenName "triphut") indexValidatorHash'
  submitTx user $ tx config spend' user

-- | A valid tx, corresponding test should pass
validIndexNftTx :: IndexNftConfig -> UserSpend -> PubKeyHash -> Tx
validIndexNftTx = mkIndexConfigNftTx True validIndexConfigNftValue

{- | Helper function for building txs
 Set the `hasDatum` flag to False to create an invalid tx that
 doesn't pay the datum to the validator script
-}
mkIndexConfigNftTx :: Bool -> (IndexNftConfig -> Value) -> IndexNftConfig -> UserSpend -> PubKeyHash -> Tx
mkIndexConfigNftTx hasDatum configValue config spend' user =
  let
    -- Set up the value and scripts
    mintVal = configValue config
    policy = indexConfigNftTypedMintingPolicy config
    validator = indexNftTypedValidator

    -- Set up the txs
    baseTx = mconcat [mintValue policy () mintVal, userSpend spend']
    withDatum = payToScript validator (InlineDatum validSampleIndexNftDatum) (adaValue 2 <> mintVal)
    withNoDatumToUser = payToKey user (adaValue 2 <> mintVal)
   in
    -- If hasDatum is set to False we want the withNoDatumToUser tx
    -- in order to trigger the negative test
    if hasDatum then baseTx <> withDatum else baseTx <> withNoDatumToUser

-- | Valid value to be used in valid tx
validIndexConfigNftValue :: IndexNftConfig -> Value
validIndexConfigNftValue nftCfg@(IndexNftConfig _ tokenName indexValidatorHash) =
  singleton (indexConfigNftCurrencySymbol nftCfg) tokenName 1
