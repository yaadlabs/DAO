{- |
Module      : Spec.Tally.Context
Description : Tally policy unit tests
-}
module Spec.Tally.Context (
  validTallyConfigNftTest,
  invalidWrongTokenNameTallyConfigNftTest,
  invalidMoreThanOneTokenMintedTallyConfigNftTest,
  invalidIndexNotIncrementedConfigNftTest,
  invalidNoConfigInRefInputsConfigNftTest,
  invalidDoesNotSpendIndexConfigNftTest,
) where

import Control.Monad (void)
import Dao.ScriptArgument (TallyNftConfig (TallyNftConfig))
import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum (IndexNftDatum))
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
import PlutusLedgerApi.V1.Value (TokenName (TokenName), Value, singleton)
import PlutusTx (fromBuiltinData, toBuiltinData)
import PlutusTx.Prelude (Maybe, ($))
import Spec.Configuration.Transactions (runInitConfig)
import Spec.Configuration.Utils (findConfig)
import Spec.Index.Script (indexNftTypedValidator)
import Spec.Index.Transactions (runInitIndex)
import Spec.Index.Utils (findIndex)
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.SampleData (sampleUpgradeWithEndTimeInFutureTallyStateDatum)
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
import Prelude (error, mconcat, mempty, show, (+), (<>))

validTallyConfigNftTest :: Run ()
validTallyConfigNftTest =
  mkTallyConfigTest
    validTallyConfigValue
    ValidIncrement
    ConfigInRefInputs
    SpendInput

invalidWrongTokenNameTallyConfigNftTest :: Run ()
invalidWrongTokenNameTallyConfigNftTest =
  mkTallyConfigTest
    invalidWrongTokenNameTallyConfigValue
    ValidIncrement
    ConfigInRefInputs
    SpendInput

invalidMoreThanOneTokenMintedTallyConfigNftTest :: Run ()
invalidMoreThanOneTokenMintedTallyConfigNftTest =
  mkTallyConfigTest
    invalidMoreThanOneTokenMintedTallyConfigValue
    ValidIncrement
    ConfigInRefInputs
    SpendInput

invalidIndexNotIncrementedConfigNftTest :: Run ()
invalidIndexNotIncrementedConfigNftTest =
  mkTallyConfigTest
    validTallyConfigValue
    DoNotIncrement
    ConfigInRefInputs
    SpendInput

invalidNoConfigInRefInputsConfigNftTest :: Run ()
invalidNoConfigInRefInputsConfigNftTest =
  mkTallyConfigTest
    validTallyConfigValue
    ValidIncrement
    NoConfigInRefInputs
    SpendInput

invalidDoesNotSpendIndexConfigNftTest :: Run ()
invalidDoesNotSpendIndexConfigNftTest =
  mkTallyConfigTest
    validTallyConfigValue
    ValidIncrement
    NoConfigInRefInputs
    DoNotSpendInput

data IncrementIndex
  = ValidIncrement
  | DoNotIncrement

data ConfigRef
  = ConfigInRefInputs
  | NoConfigInRefInputs

data SpendIndexInput
  = SpendInput
  | DoNotSpendInput

mkTallyConfigTest ::
  (TallyNftConfig -> Value) ->
  IncrementIndex ->
  ConfigRef ->
  SpendIndexInput ->
  Run ()
mkTallyConfigTest tallyConfigValue incrementIndex configRef spendIndex = do
  void runInitConfig
  void runInitIndex

  (configOutRef, _, _) <- findConfig
  (indexOutRef, _, indexDatum) <- findIndex

  -- logInfo' $ show indexDatum
  let builtinIndex = toBuiltinData indexDatum
  let
    fromBuiltinIndex :: Maybe IndexNftDatum
    fromBuiltinIndex = fromBuiltinData builtinIndex

  -- error $ show $ fromBuiltinIndex

  user <- newUser $ amountOfAda 4_000_000
  spend1 <- spend user (adaValue 2_000_002)
  spend2 <- spend user (adaValue 4_000_000)

  let config =
        TallyNftConfig
          dummyIndexConfigNftSymbol
          dummyIndexConfigNftTokenName
          dummyConfigNftSymbol
          dummyConfigNftTokenName

  let
    -- Set up the value and scripts
    tallyValue :: Value
    tallyValue = tallyConfigValue config

    tallyPolicy :: TypedPolicy ()
    tallyPolicy = tallyConfigNftTypedMintingPolicy config

    -- Valid output index datum should have index field incremented by one
    updateIndexDatum :: IndexNftDatum -> IndexNftDatum
    updateIndexDatum oldDatum@(IndexNftDatum indexNftDatum'index) = case incrementIndex of
      ValidIncrement -> IndexNftDatum $ indexNftDatum'index + 1
      DoNotIncrement -> oldDatum

    -- Set up the txs
    baseTx =
      mconcat
        [ mintValue tallyPolicy () tallyValue
        , -- \^ Mint the tally NFT
          userSpend spend1
        , userSpend spend2
        -- \^ Spend these to balance the tx
        ]

    -- Valid tx has the config in the reference inputs
    withReferenceConfig = case configRef of
      ConfigInRefInputs -> refInputInline configOutRef
      NoConfigInRefInputs -> mempty

    -- Valid tx spends the input at the index validator with the old datum,
    withIndexInput = case spendIndex of
      SpendInput -> spendScript indexNftTypedValidator indexOutRef () indexDatum
      DoNotSpendInput -> mempty

    -- Pay the tally datum, and token,
    -- to the tally validator
    payToTallyValidator =
      payToScript
        tallyNftTypedValidator
        (InlineDatum sampleUpgradeWithEndTimeInFutureTallyStateDatum)
        (adaValue 2 <> tallyValue)

    -- Pay the updated index datum with the incremented
    -- index field, and token, to the index validator
    payToIndexValidator =
      payToScript
        indexNftTypedValidator
        (InlineDatum $ updateIndexDatum indexDatum)
        (amountOfAda 4_000_000 <> dummyIndexConfigNftValue)

    -- Combine the txs
    allTxs =
      mconcat
        [ baseTx
        , withReferenceConfig
        , withIndexInput
        , payToTallyValidator
        , payToIndexValidator
        ]

  submitTx user allTxs

-- Valid token value, exactly one minted, correct symbol and token name set to index field of index datum
validTallyConfigValue :: TallyNftConfig -> Value
validTallyConfigValue config = singleton (tallyConfigNftCurrencySymbol config) (TokenName "0") 1

invalidWrongTokenNameTallyConfigValue :: TallyNftConfig -> Value
invalidWrongTokenNameTallyConfigValue config =
  singleton (tallyConfigNftCurrencySymbol config) (TokenName "some_wrong_name") 1

invalidMoreThanOneTokenMintedTallyConfigValue :: TallyNftConfig -> Value
invalidMoreThanOneTokenMintedTallyConfigValue config =
  singleton (tallyConfigNftCurrencySymbol config) (TokenName "some_wrong_name") 2
