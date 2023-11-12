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
  payToScript,
  refInputInline,
  spendScript,
 )
import Plutus.V1.Ledger.Value (TokenName (TokenName), Value, singleton)
import PlutusTx.Prelude (($))
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.Index.Script (indexNftTypedValidator)
import Spec.Index.Transactions (runInitIndex)
import Spec.Index.Utils (findIndex)
import Spec.SpecUtils (minAda)
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
import Triphut.Tally (TallyNftConfig (TallyNftConfig))
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
        , -- \^ Mint the tally NFT
          spendScript indexNftTypedValidator indexOutRef () indexDatum
        , -- \^ Spend the input at the index validator with the old datum,
          -- adds the index to the inputs
          refInputInline configOutRef
        , -- \^ Add the config to the reference inputs
          userSpend spend1
        , userSpend spend2
        -- \^ Spend these to balance the tx
        ]

    -- Pay the tally datum, and token,
    -- to the tally validator
    payToTallyValidator =
      payToScript
        tallyNftTypedValidator
        (InlineDatum sampleTallyStateDatum)
        (adaValue 2 <> tallyConfigValue)

    -- Pay the updated index datum with the incremented
    -- index field, and token, to the index validator
    payToIndexValidator =
      payToScript
        indexNftTypedValidator
        (InlineDatum $ updateIndexDatum indexDatum)
        (adaValue 2 <> dummyIndexConfigNftValue)

    -- Combine the txs
    allTxs = mconcat [baseTx, payToTallyValidator, payToIndexValidator]

  submitTx user allTxs
