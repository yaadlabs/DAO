module Canonical.Shared where
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx.AssocMap as M
import           PlutusTx.Prelude
import qualified Cardano.Api.Shelley as Shelly
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Codec.Serialise (serialise)

type WrappedMintingPolicyType = BuiltinData -> BuiltinData -> ()

{-# INLINABLE extractDatumBytes #-}
extractDatumBytes :: [(DatumHash, Datum)] -> DatumHash -> BuiltinData
extractDatumBytes datums dh = getDatum $ extractDatum datums dh

{-# INLINABLE extractDatum #-}
extractDatum :: [(DatumHash, Datum)] -> DatumHash -> Datum
extractDatum datums dh = go datums where
  go = \case
    [] -> traceError "Failed to find datum"
    (x, y):xs ->
      if x == dh then
        y
      else
        go xs

{-# INLINABLE hasSingleToken #-}
hasSingleToken :: Value -> CurrencySymbol -> TokenName -> Bool
hasSingleToken (Value v) s t = case M.lookup s v of
  Just m -> case M.toList m of
    [(t', c)] -> t' == t && c == 1
    _ -> traceError "wrong number of tokens with policy id"
  _ -> False

toCardanoApiScript :: Script -> Shelly.Script Shelly.PlutusScriptV2
toCardanoApiScript
  = Shelly.PlutusScript Shelly.PlutusScriptV2
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
mintingPolicyHash
  = MintingPolicyHash
  . getScriptHash
  . scriptHash
  . getValidator
  . Validator
  . getMintingPolicy
