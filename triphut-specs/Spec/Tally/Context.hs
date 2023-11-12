{- |
Module      : Spec.Tally.Context
Description : Tally policy context unit tests
-}
module Spec.Tally.Context (validTallyConfigNftTest) where

import Control.Monad (void)
import Plutus.Model (
  Run,
  TypedPolicy,
  adaValue,
  mintValue,
  newUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  TxBox,
  payToScript,
  refInputInline,
  spendScript,
  txBoxValue,
 )
import Plutus.V1.Ledger.Value (TokenName (TokenName), Value, singleton)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import PlutusTx.Prelude (Bool, ($))
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
import Prelude (mconcat, (+), (<>))

validTallyConfigNftTest :: Run ()
validTallyConfigNftTest = do
  void runInitConfig
  void runInitIndex

  (configOutRef, _, _) <- findConfig
  (indexOutRef, _, indexDatum) <- findIndex

  user <- newUser minAda
  spend1 <- spend user (adaValue 2)
  spend2 <- spend user (adaValue 2)

  let config =
        TallyNftConfig
          dummyIndexConfigNftSymbol
          dummyIndexConfigNftTokenName
          dummyConfigNftSymbol
          dummyConfigNftTokenName

  let
    -- Set up the value and scripts
    tallyConfigValue :: Value
    tallyConfigValue = singleton (tallyConfigNftCurrencySymbol config) (TokenName "0") 1

    tallyPolicy :: TypedPolicy ()
    tallyPolicy = tallyConfigNftTypedMintingPolicy config

    updateIndexDatum :: IndexNftDatum -> IndexNftDatum
    updateIndexDatum (IndexNftDatum index) = IndexNftDatum $ index + 1

    -- Set up the txs
    baseTx =
      mconcat
        [ mintValue tallyPolicy () tallyConfigValue
        , userSpend spend1
        , userSpend spend2
        , spendScript indexNftTypedValidator indexOutRef () indexDatum
        , refInputInline configOutRef
        ]

    payToTallyValidator =
      payToScript
        tallyNftTypedValidator
        (InlineDatum sampleTallyStateDatum)
        (adaValue 2 <> tallyConfigValue)

    payToIndexValidator =
      payToScript
        indexNftTypedValidator
        (InlineDatum $ updateIndexDatum indexDatum)
        (adaValue 2 <> dummyIndexConfigNftValue)

  submitTx user $ baseTx <> payToTallyValidator <> payToIndexValidator

findConfig :: Run (TxOutRef, TxOut, DynamicConfigDatum)
findConfig = findUniqueUtxo alwaysSucceedTypedValidator check
  where
    check :: TxBox AlwaysSucceedScript -> Bool
    check box = hasOneOfToken dummyConfigNftSymbol dummyConfigNftTokenName (txBoxValue box)

findIndex :: Run (TxOutRef, TxOut, IndexNftDatum)
findIndex = findUniqueUtxo indexNftTypedValidator check
  where
    check :: TxBox IndexValidatorScript -> Bool
    check box = hasOneOfToken dummyIndexConfigNftSymbol dummyIndexConfigNftTokenName (txBoxValue box)
