{- |
Module: Dao.Shared
Description: Contains helper functions used across the other modules.
-}
module Dao.Shared (
  WrappedMintingPolicyType,
  hasTokenInValueNoErrors,
  -- validatorToScript,
  -- policyToScript,
  -- mkValidatorWithSettings,
  wrapValidate,
  wrapValidate',
  wrapValidate'',
  hasBurnedTokens,
  hasTokenInValue,
  getTokenNameOfNft,
  countOfTokenInValue,
  convertDatum,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  hasOneOfToken,
  integerToByteString,
  isScriptCredential,
  lovelacesOf,
  -- mintingPolicyHash,
  -- plutonomyMintingPolicyHash,
  -- validatorHash,
) where

-- import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
-- import Cardano.Api.Shelley qualified as Shelly
import Codec.Serialise (serialise)

import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS

import Plutonomy qualified
import PlutusLedgerApi.V1.Credential (Credential (ScriptCredential))

-- import Plutus.V1.Ledger.Scripts (
--   Datum (Datum),
--   DatumHash,
--   MintingPolicy,
--   MintingPolicyHash (MintingPolicyHash),
--   Script,
--   ScriptHash (ScriptHash),
--   Validator (Validator),
--   ValidatorHash (ValidatorHash),
--   getMintingPolicy,
--   getScriptHash,
--   getValidator,
--   unMintingPolicyScript,
--  )

import PlutusLedgerApi.V1 (CurrencySymbol)
import PlutusLedgerApi.V1.Value (TokenName, Value (Value, getValue), adaSymbol, adaToken)
import PlutusLedgerApi.V2 (
  Datum (Datum),
  DatumHash,
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
 )

import Data.Maybe (fromJust)
import PlutusTx (FromData, UnsafeFromData, fromBuiltinData, unsafeFromBuiltinData)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinByteString,
  BuiltinData,
  BuiltinString,
  Integer,
  Maybe (Just, Nothing),
  check,
  divide,
  fromMaybe,
  isJust,
  modulo,
  otherwise,
  toBuiltin,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
  (<),
  (<>),
  (==),
 )

type WrappedMintingPolicyType = BuiltinData -> BuiltinData -> ()

{-# INLINEABLE isScriptCredential #-}
isScriptCredential :: Credential -> Bool
isScriptCredential = \case
  ScriptCredential _ -> True
  _ -> False

{-# INLINEABLE hasSymbolInValue #-}
hasSymbolInValue :: CurrencySymbol -> Value -> Bool
hasSymbolInValue symbol = isJust . Map.lookup symbol . getValue

{- | Checks that the given `Value` contains exactly one token
    with the given `CurrencySymbol` and `TokenName`
-}
{-# INLINEABLE hasSingleTokenWithSymbolAndTokenName #-}
hasSingleTokenWithSymbolAndTokenName :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleTokenWithSymbolAndTokenName (Value value) symbol tokenName = case Map.lookup symbol value of
  Nothing -> False
  Just map' -> case Map.toList map' of
    [(tn, c)] ->
      traceIfFalse "Incorrect token name provided" (tn == tokenName)
        && traceIfFalse "Should be exactly one token" (c == 1)
    _ -> traceError "Wrong number of tokens with policy id"

{- | Return True if the value contains exactly one of the given token
  Same as `hasTokenInValue` but contains error traces (traceError)
-}
hasTokenInValueNoErrors :: CurrencySymbol -> Value -> Bool
hasTokenInValueNoErrors symbol (Value value) = case Map.lookup symbol value of
  Nothing -> False
  Just map' -> case Map.toList map' of
    [(_, c)] -> c == 1
    _ -> False

{- | Return `Just TokenName` if the value contains exactly one of the given token
 Returns `Maybe` in order to be used by the separate
 `hasTokenInValue` and `getTokenNameOfNft` helpers
-}
getTokenNameOfNftMaybe :: CurrencySymbol -> BuiltinString -> Value -> Maybe TokenName
getTokenNameOfNftMaybe symbol errorMessage (Value value) = case Map.lookup symbol value of
  Nothing -> traceError $ errorMessage <> ": Symbol not found"
  Just map' -> case Map.toList map' of
    [(tokenName, c)]
      | c == 1 -> Just tokenName
      | otherwise -> traceError $ errorMessage <> ": Token count should be exactly one"
    _ -> traceError $ errorMessage <> ": Incorrect number of tokens"

-- | Return true if the value contains exactly one of the given token
hasTokenInValue :: CurrencySymbol -> BuiltinString -> Value -> Bool
hasTokenInValue symbol errorMessage = isJust . getTokenNameOfNftMaybe symbol errorMessage

-- | Retrieve the token name of corresponding symbol from value
getTokenNameOfNft :: CurrencySymbol -> Value -> BuiltinString -> TokenName
getTokenNameOfNft symbol value errorMessage =
  fromMaybe (traceError $ errorMessage <> ": not found") (getTokenNameOfNftMaybe symbol errorMessage value)

-- | Check that tokens were burned, otherwise trace the specific error
hasBurnedTokens :: CurrencySymbol -> Value -> BuiltinString -> Bool
hasBurnedTokens symbol (Value value) errorMessage =
  case Map.lookup symbol value of
    Nothing -> traceError $ errorMessage <> ": Symbol not found"
    Just map' -> case Map.toList map' of
      [(_, c)] -> traceIfFalse (errorMessage <> ": Count is not less than zero") (c < 0)
      _ -> traceError $ errorMessage <> ": Wrong number of tokens"

{- | Get the count of tokens with the given `CurrencySymbol`
 and `TokenName` in the given `Value`
-}
countOfTokenInValue :: CurrencySymbol -> TokenName -> Value -> Integer
countOfTokenInValue symbol tokenName (Value value) =
  case Map.lookup symbol value of
    Nothing -> 0
    Just map' -> fromMaybe 0 $ Map.lookup tokenName map'

-- | Get the count of lovelaces in the given `Value`
lovelacesOf :: Value -> Integer
lovelacesOf = countOfTokenInValue adaSymbol adaToken

{-# INLINEABLE hasOneOfToken #-}
hasOneOfToken :: CurrencySymbol -> TokenName -> Value -> Bool
hasOneOfToken symbol tokenName (Value value) = case Map.lookup symbol value of
  Just map' -> case Map.lookup tokenName map' of
    Just c -> c == 1
    Nothing -> False
  Nothing -> False

{-# INLINEABLE convertDatum #-}
convertDatum :: (FromData a) => Map DatumHash Datum -> OutputDatum -> a
convertDatum infoData datum = case datum of
  OutputDatum (Datum dbs) -> case fromBuiltinData dbs of
    Just dbs' -> dbs'
    Nothing -> traceError "convertDatum: Error at fromBuiltinData"
  OutputDatumHash dh -> case Map.lookup dh infoData of
    Just (Datum dbs) -> case fromBuiltinData dbs of
      Just dbs' -> dbs'
      Nothing -> traceError "convertDatum: Error at fromBuiltinData"
    _ -> traceError "convertDatum: Missing datum"
  NoOutputDatum -> traceError "convertDatum: Missing datum hash or datum"

{-# INLINEABLE integerToByteString #-}
integerToByteString :: Integer -> BuiltinByteString
integerToByteString n
  | n == 0 = "0"
  | n == 1 = "1"
  | n == 2 = "2"
  | n == 3 = "3"
  | n == 4 = "4"
  | n == 5 = "5"
  | n == 6 = "6"
  | n == 7 = "7"
  | n == 8 = "8"
  | n == 9 = "9"
  | otherwise =
      integerToByteString (n `divide` 10)
        <> integerToByteString (n `modulo` 10)

-- | Transforms a validator function `validate` to its lower level representation
wrapValidate' ::
  (FromData b, UnsafeFromData c, UnsafeFromData d) =>
  (config -> b -> c -> d -> Bool) ->
  config ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidate' validate config x y z =
  let maybeDataArg = fromBuiltinData x
   in case maybeDataArg of
        Nothing -> traceError "wrapValidate': Error at fromBuiltinData"
        Just dataArg ->
          check
            ( validate
                config
                dataArg
                (unsafeFromBuiltinData y)
                (unsafeFromBuiltinData z)
            )

wrapValidate'' ::
  (FromData b, FromData c, UnsafeFromData d) =>
  (config -> b -> c -> d -> Bool) ->
  config ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidate'' validate config x y z =
  let (maybeDataArgX, maybeDataArgY) = (fromBuiltinData x, fromBuiltinData y)
   in case (maybeDataArgX, maybeDataArgY) of
        (Just dataArgX, Just dataArgY) ->
          check
            ( validate
                config
                dataArgX
                dataArgY
                (unsafeFromBuiltinData z)
            )
        _ -> traceError "wrapValidate'': Error at fromBuiltinData"

wrapValidate ::
  (UnsafeFromData b, UnsafeFromData c, UnsafeFromData d) =>
  (config -> b -> c -> d -> Bool) ->
  config ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidate validate config x y z =
  check
    ( validate
        config
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

{- | Make Validator with given Plutonomy optimisations
mkValidatorWithSettings ::
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) ->
  Bool ->
  Validator
mkValidatorWithSettings
  compiledCode
  setFloatOutputLambda =
    let
      optimizerSettings =
        Plutonomy.defaultOptimizerOptions
          { Plutonomy.ooSplitDelay = False
          , Plutonomy.ooFloatOutLambda = setFloatOutputLambda
          }
     in
      Plutonomy.optimizeUPLCWith optimizerSettings
        $ Plutonomy.validatorToPlutus
        $ Plutonomy.validatorToRaw
        $ Plutonomy.mkValidatorScript compiledCode
-}

{- | Convert validator with a config to Plutus script
validatorToScript :: (config -> Validator) -> config -> PlutusScript PlutusScriptV2
validatorToScript f config =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    $ f config
-}

{- | Convert policy with a config to Plutus script
policyToScript :: (config -> MintingPolicy) -> config -> PlutusScript PlutusScriptV2
policyToScript f =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . Validator
    . unMintingPolicyScript
    . f
-}

-- toCardanoApiScript :: Script -> Shelly.Script Shelly.PlutusScriptV2
-- toCardanoApiScript =
--   Shelly.PlutusScript Shelly.PlutusScriptV2
--     . Shelly.PlutusScriptSerialised
--     . BSS.toShort
--     . BSL.toStrict
--     . serialise
--
-- scriptHash :: Script -> ScriptHash
-- scriptHash =
--   ScriptHash
--     . toBuiltin
--     . Shelly.serialiseToRawBytes
--     . Shelly.hashScript
--     . toCardanoApiScript
--
-- mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
-- mintingPolicyHash =
--   MintingPolicyHash
--     . getScriptHash
--     . scriptHash
--     . getValidator
--     . Validator
--     . getMintingPolicy
--
-- plutonomyMintingPolicyHash :: MintingPolicy -> MintingPolicyHash
-- plutonomyMintingPolicyHash =
--   let
--     optimizerSettings =
--       Plutonomy.defaultOptimizerOptions
--         { Plutonomy.ooSplitDelay = False
--         , Plutonomy.ooFloatOutLambda = False
--         }
--    in
--     MintingPolicyHash
--       . getScriptHash
--       . scriptHash
--       . getValidator
--       . Plutonomy.optimizeUPLCWith optimizerSettings
--       . Validator
--       . getMintingPolicy
--
-- validatorHash :: Validator -> ValidatorHash
-- validatorHash = ValidatorHash . getScriptHash . scriptHash . getValidator
