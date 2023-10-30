module Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasSingleToken,
  mintingPolicyHash,
  plutonomyMintingPolicyHash,
  validatorHash,
) where

import Cardano.Api.Shelley qualified as Shelly
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
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
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value (Value))
import Plutus.V2.Ledger.Tx (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
import PlutusTx (UnsafeFromData, unsafeFromBuiltinData)
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import PlutusTx.Prelude (
  Bool (False),
  BuiltinData,
  Maybe (Just),
  toBuiltin,
  traceError,
  ($),
  (&&),
  (.),
  (==),
 )

type WrappedMintingPolicyType = BuiltinData -> BuiltinData -> ()

{-# INLINEABLE hasSingleToken #-}
hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken (Value v) s t = case M.lookup s v of
  Just m -> case M.toList m of
    [(t', c)] -> t' == t && c == 1
    _ -> traceError "wrong number of tokens with policy id"
  _ -> False

{-# INLINEABLE convertDatum #-}
convertDatum :: UnsafeFromData a => Map DatumHash Datum -> OutputDatum -> a
convertDatum infoData datum = unsafeFromBuiltinData $ case datum of
  OutputDatum (Datum dbs) -> dbs
  OutputDatumHash dh -> case M.lookup dh infoData of
    Just (Datum dbs) -> dbs
    _ -> traceError "Missing datum"
  NoOutputDatum -> traceError "Missing datum hash or datum"

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
