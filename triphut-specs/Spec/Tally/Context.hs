{- |
Module      : Spec.Tally.Context
Description : Tally policy context unit tests
-}
module Spec.Tally.Context (validTallyConfigNftTest) where

import Plutus.Model (
  Run,
  Tx,
  UserSpend,
  adaValue,
  mintValue,
  newUser,
  spend,
  submitTx,
  userSpend,
  utxoAt,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  TxBox,
  payToScript,
  refInputInline,
  spendScript,
  txBoxValue,
 )
import Plutus.V1.Ledger.Value (Value, singleton)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import PlutusTx.Prelude (Bool, error, ($))
import Spec.AlwaysSucceed.Script (AlwaysSucceedScript, alwaysSucceedTypedValidator)
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.Index.Script (IndexValidatorScript, indexNftTypedValidator)
import Spec.Index.Transactions (runInitIndex)
import Spec.SpecUtils (findUniqueUtxo, minAda)
import Spec.Tally.SampleData (sampleTallyStateDatum)
import Spec.Tally.Script (
  tallyConfigNftCurrencySymbol,
  tallyConfigNftTypedMintingPolicy,
  tallyNftTypedValidator,
 )
import Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyIndexConfigNftSymbol,
  dummyIndexConfigNftTokenName,
  dummyIndexConfigNftValue,
 )
import Triphut.Index (IndexNftDatum (IndexNftDatum))
import Triphut.Shared (hasOneOfToken)
import Triphut.Tally (TallyNftConfig (TallyNftConfig))
import Triphut.Types (DynamicConfigDatum)
import Prelude (mconcat, pure, (+), (<>))

-- | Valid test
validTallyConfigNftTest :: Run ()
validTallyConfigNftTest = mkTallyConfigNftTest validTallyNftTx

-- | Helper function for making tests
mkTallyConfigNftTest :: (TallyNftConfig -> UserSpend -> Run Tx) -> Run ()
mkTallyConfigNftTest tx = do
  _ <- runInitConfig
  _ <- runInitIndex
  user <- newUser minAda
  spend' <- spend user (adaValue 4)
  let config =
        TallyNftConfig
          dummyIndexConfigNftSymbol
          dummyIndexConfigNftTokenName
          dummyConfigNftSymbol
          dummyConfigNftTokenName
  tx' <- tx config spend'
  submitTx user tx'

-- | A valid tx, corresponding test should pass
validTallyNftTx :: TallyNftConfig -> UserSpend -> Run Tx
validTallyNftTx = mkTallyConfigNftTx validTallyConfigNftValue

mkTallyConfigNftTx ::
  (TallyNftConfig -> Value) ->
  TallyNftConfig ->
  UserSpend ->
  Run Tx
mkTallyConfigNftTx configValue config spend' = do
  (configOutRef, _, _) <- findConfig
  (indexOutRef, _, indexDatum) <- findIndex

  let
    -- Set up the value and scripts
    mintVal = configValue config
    policy = tallyConfigNftTypedMintingPolicy config

    updateIndexDatum :: IndexNftDatum -> IndexNftDatum
    updateIndexDatum (IndexNftDatum index) = IndexNftDatum $ index + 1

    -- Set up the txs
    baseTx =
      mconcat
        [ mintValue policy () mintVal
        , userSpend spend'
        , refInputInline configOutRef
        , spendScript indexNftTypedValidator indexOutRef () indexDatum
        ]

    payToTallyValidator =
      payToScript
        tallyNftTypedValidator
        (InlineDatum sampleTallyStateDatum)
        (adaValue 2 <> mintVal)

    payToIndexValidator =
      payToScript
        indexNftTypedValidator
        (InlineDatum $ updateIndexDatum indexDatum)
        (adaValue 2 <> dummyIndexConfigNftValue)
   in
    pure $ baseTx <> payToTallyValidator <> payToIndexValidator

findConfig :: Run (TxOutRef, TxOut, DynamicConfigDatum)
findConfig = findUniqueUtxo alwaysSucceedTypedValidator check
  where
    check :: TxBox AlwaysSucceedScript -> Bool
    check box = hasOneOfToken dummyConfigNftSymbol "config" (txBoxValue box)

findIndex :: Run (TxOutRef, TxOut, IndexNftDatum)
findIndex = findUniqueUtxo indexNftTypedValidator check
  where
    check :: TxBox IndexValidatorScript -> Bool
    check box = hasOneOfToken dummyIndexConfigNftSymbol dummyIndexConfigNftTokenName (txBoxValue box)

-- | Valid value to be used in valid tx
validTallyConfigNftValue :: TallyNftConfig -> Value
validTallyConfigNftValue nftCfg@(TallyNftConfig _ tokenName _ _) =
  singleton (tallyConfigNftCurrencySymbol nftCfg) tokenName 1
