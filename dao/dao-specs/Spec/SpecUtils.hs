module Spec.SpecUtils (
  runInitPayToScript,
  runInitReferenceScript,
  checkFails,
  mkTypedValidator',
  getFirstRefScript,
  minAda,
  amountOfAda,
  findUniqueUtxo,
  findConfigUtxo,
  oneSecond,
) where

import Control.Monad (unless, void)
import Dao.Shared (hasOneOfToken)
import Plutus.Model (
  Ada (Lovelace),
  IsValidator,
  MockConfig,
  Run,
  ada,
  getMainUser,
  mustFail,
  payToRef,
  payToScript,
  sendTx,
  signTx,
  skipLimits,
  spend,
  submitTx,
  testNoErrors,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  DatumType,
  HasAddress,
  HasDatum,
  HasRedeemer,
  TxBox,
  TypedValidator (TypedValidator),
  boxAt,
  checkErrors,
  loadRefScript,
  mkTypedValidator,
  refScriptAt,
  toV2,
  txBoxDatum,
  txBoxOut,
  txBoxRef,
  txBoxValue,
 )
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime))
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName, Value)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
import PlutusTx (CompiledCode)
import PlutusTx.Prelude (Bool, BuiltinData, Integer, Maybe (Just, Nothing), fst, head, ($), (.), (>>=))
import Test.Tasty (TestTree)
import Prelude (Eq, String, error, mconcat, pure, show, (<$>), (<>), (==))

checkFails :: MockConfig -> Value -> String -> Run () -> TestTree
checkFails cfg funds msg act =
  testNoErrors funds (skipLimits cfg) msg (mustFail act)

mkTypedValidator' ::
  (config -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())) ->
  config ->
  TypedValidator datum redeemer
mkTypedValidator' mkValidator = mkTypedValidator . mkValidator

initScriptRef :: (IsValidator script) => script -> Run ()
initScriptRef script = do
  admin <- getMainUser
  sp <- spend admin minAda
  let tx =
        mconcat
          [ userSpend sp
          , loadRefScript script admin minAda
          ]
  void $ signTx admin tx >>= sendTx

data ScriptType = Reference | Script
  deriving stock (Eq)

runInitScript ::
  (IsValidator script) =>
  script ->
  ScriptType ->
  DatumType script ->
  Value ->
  Run ()
runInitScript validatorScript scriptType datum token = do
  unless (scriptType == Script) (initScriptRef validatorScript)
  admin <- getMainUser
  let value = minAda <> token
  spend' <- spend admin value
  let payTx = case scriptType of
        Reference -> payToRef validatorScript (InlineDatum datum) value
        Script -> payToScript validatorScript (InlineDatum datum) value
  submitTx admin $ payTx <> userSpend spend'

runInitPayToScript ::
  (IsValidator script) =>
  script ->
  DatumType script ->
  Value ->
  Run ()
runInitPayToScript script = runInitScript script Script

runInitReferenceScript ::
  (IsValidator script) =>
  script ->
  DatumType script ->
  Value ->
  Run ()
runInitReferenceScript script = runInitScript script Reference

getFirstRefScript :: (IsValidator script) => script -> Run TxOutRef
getFirstRefScript script =
  fst . head <$> refScriptAt script

minAda :: Value
minAda = ada $ Lovelace 2_000_000

amountOfAda :: Integer -> Value
amountOfAda = ada . Lovelace

findConfigUtxo ::
  (IsValidator script) =>
  script ->
  CurrencySymbol ->
  TokenName ->
  Run (TxOutRef, TxOut, DatumType script)
findConfigUtxo validatorScript symbol tokenName = findUniqueUtxo validatorScript check
  where
    check :: TxBox script -> Bool
    check box = hasOneOfToken symbol tokenName (txBoxValue box)

findUniqueUtxo ::
  (IsValidator script) =>
  script ->
  (TxBox script -> Bool) ->
  Run (TxOutRef, TxOut, DatumType script)
findUniqueUtxo validator selector =
  findUniqueUtxo' validator selector
    >>= ( \case
            Nothing -> do
              errs <- checkErrors
              error ("Either no or more than 1 utxos found. Previous errors: " <> show errs)
            Just x0 -> pure x0
        )

findUniqueUtxo' ::
  (IsValidator script) =>
  script ->
  (TxBox script -> Bool) ->
  Run (Maybe (TxOutRef, TxOut, DatumType script))
findUniqueUtxo' validator selector = do
  boxes <- boxAt validator
  let res = [(txBoxRef a, txBoxOut a, txBoxDatum a) | a <- boxes, selector a]
  case res of
    [(oref, o, datum)] -> pure $ Just (oref, o, datum)
    _ -> pure Nothing

oneSecond :: POSIXTime
oneSecond = POSIXTime 1_000
