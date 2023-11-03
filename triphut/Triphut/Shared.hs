module Triphut.Shared (
  WrappedMintingPolicyType,
  hasBurnedTokens,
  hasTokenInValue,
  getTokenNameOfNft,
  countOfTokenInValue,
  convertDatum,
  hasSingleToken,
  hasSymbolInValue,
  hasOneOfToken,
  integerToByteString,
  isScriptCredential,
  lovelacesOf,
  mintingPolicyHash,
  plutonomyMintingPolicyHash,
  validatorHash,
) where

import Cardano.Api.Shelley qualified as Shelly
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash,
  MintingPolicy,
  MintingPolicyHash (MintingPolicyHash),
  Script,
  ScriptHash (ScriptHash),
  Validator (Validator),
  ValidatorHash (ValidatorHash),
  getMintingPolicy,
  getScriptHash,
  getValidator,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value (Value), adaSymbol, adaToken)
import Plutus.V2.Ledger.Tx (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
import PlutusTx (UnsafeFromData, unsafeFromBuiltinData)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinByteString,
  BuiltinData,
  BuiltinString,
  Integer,
  Maybe (Just, Nothing),
  const,
  divide,
  id,
  maybe,
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
hasSymbolInValue symbol (Value value) = maybe False (const True) (Map.lookup symbol value)

{-# INLINEABLE hasSingleToken #-}
hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken (Value value) symbol tokenName = case Map.lookup symbol value of
  Nothing -> False
  Just map' -> case Map.toList map' of
    [(tn, c)] ->
      traceIfFalse "Wrong token name" (tn == tokenName)
        && traceIfFalse "Should be exactly one" (c == 1)
    _ -> traceError "Wrong number of tokens with policy id"

{- | Return `Maybe TokenName` if the value contains exactly one of the given token
 Returns `Maybe` in order to be used by the separate
 `hasTokenInValue` and `getTokenNameOfNft` helpers
-}
getTokenNameOfNftMaybe :: CurrencySymbol -> Value -> BuiltinString -> Maybe TokenName
getTokenNameOfNftMaybe symbol (Value value) specificToken = case Map.lookup symbol value of
  Nothing -> traceError $ specificToken <> ": Symbol not found"
  Just map' -> case Map.toList map' of
    [(tokenName, c)]
      | c == 1 -> Just tokenName
      | otherwise -> traceError $ specificToken <> ": Token count should be exactly one"
    _ -> traceError $ specificToken <> ": Incorrect number of tokens"

-- | Return true if the value contains exactly one of the given token
hasTokenInValue :: CurrencySymbol -> BuiltinString -> Value -> Bool
hasTokenInValue symbol specificToken value =
  maybe False (const True) (getTokenNameOfNftMaybe symbol value specificToken)

-- | Retrive the token name of corresponding symbol from value
getTokenNameOfNft :: CurrencySymbol -> Value -> BuiltinString -> TokenName
getTokenNameOfNft symbol value specificToken =
  maybe (traceError $ specificToken <> ": not found") id (getTokenNameOfNftMaybe symbol value specificToken)

-- | Check that tokens were burned, otherwise trace the specific error
hasBurnedTokens :: CurrencySymbol -> Value -> BuiltinString -> Bool
hasBurnedTokens symbol (Value value) specificMessage =
  case Map.lookup symbol value of
    Nothing -> traceError $ specificMessage <> ": Symbol not found"
    Just map' -> case Map.toList map' of
      [(_, c)] -> traceIfFalse (specificMessage <> ": Count is not less than zero") (c < 0)
      _ -> traceError $ specificMessage <> ": Wrong number of tokens"

{- | Get the count of tokens with the given `CurrencySymbol`
 and `TokenName` in the given `Value`
-}
countOfTokenInValue :: CurrencySymbol -> TokenName -> Value -> Integer
countOfTokenInValue symbol tokenName (Value value) =
  case Map.lookup symbol value of
    Nothing -> 0
    Just map' -> case Map.lookup tokenName map' of
      Nothing -> 0
      Just c -> c

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
convertDatum :: UnsafeFromData a => Map DatumHash Datum -> OutputDatum -> a
convertDatum infoData datum = unsafeFromBuiltinData $ case datum of
  OutputDatum (Datum dbs) -> dbs
  OutputDatumHash dh -> case Map.lookup dh infoData of
    Just (Datum dbs) -> dbs
    _ -> traceError "Missing datum"
  NoOutputDatum -> traceError "Missing datum hash or datum"

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

toCardanoApiScript :: Script -> Shelly.Script Shelly.PlutusScriptV2
toCardanoApiScript =
  Shelly.PlutusScript Shelly.PlutusScriptV2
    . Shelly.PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise

scriptHash :: Script -> ScriptHash
scriptHash =
  ScriptHash
    . toBuiltin
    . Shelly.serialiseToRawBytes
    . Shelly.hashScript
    . toCardanoApiScript

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash =
  MintingPolicyHash
    . getScriptHash
    . scriptHash
    . getValidator
    . Validator
    . getMintingPolicy

plutonomyMintingPolicyHash :: MintingPolicy -> MintingPolicyHash
plutonomyMintingPolicyHash =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        , Plutonomy.ooFloatOutLambda = False
        }
   in
    MintingPolicyHash . getScriptHash . scriptHash . getValidator . Plutonomy.optimizeUPLCWith optimizerSettings . Validator . getMintingPolicy

validatorHash :: Validator -> ValidatorHash
validatorHash = ValidatorHash . getScriptHash . scriptHash . getValidator
