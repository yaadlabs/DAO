module Spec.Vote.Script (
  -- * Minting policy
  VoteMintingPolicy,
  voteTypedMintingPolicy,
  voteCurrencySymbol,
  voteValue,

  -- * Validator
  VoteValidatorScript,
  voteTypedValidator,
  voteValidatorScriptHash,
)
where

import Dao.ScriptArgument (ValidatorParams (ValidatorParams))
import Dao.Vote.Script (mkVoteMinter, validateVote)
import LambdaBuffers.ApplicationTypes.Vote (
  VoteActionRedeemer,
  VoteDatum,
  VoteMinterActionRedeemer,
 )
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  mkTypedValidator,
  scriptCurrencySymbol,
  scriptHash,
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (
  BuiltinData,
  Maybe (Just),
  check,
  traceError,
  ($),
  (.),
 )
import Spec.Configuration.SampleData (sampleValidatorParams)
import Spec.SpecUtils (mkUntypedValidator')

-- Policy script and info
type VoteMintingPolicy = TypedPolicy VoteMinterActionRedeemer

voteTypedMintingPolicy :: ValidatorParams -> VoteMintingPolicy
voteTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||wrappedPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

wrappedPolicy :: ValidatorParams -> (BuiltinData -> BuiltinData -> ())
wrappedPolicy config x y =
  let (maybeDataX, maybeDataY) = (PlutusTx.fromBuiltinData x, PlutusTx.fromBuiltinData y)
   in case (maybeDataX, maybeDataY) of
        (Just dataX, Just dataY) -> check (mkVoteMinter config dataX dataY)
        _ -> traceError "Error at fromBuiltinData function"

voteCurrencySymbol :: ValidatorParams -> CurrencySymbol
voteCurrencySymbol = scriptCurrencySymbol . voteTypedMintingPolicy

voteValue :: ValidatorParams -> Value
voteValue voteCfg@(ValidatorParams _ tokenName) = singleton (voteCurrencySymbol voteCfg) tokenName 1

-- Validator script and info
type VoteValidatorScript = TypedValidator VoteDatum VoteActionRedeemer

voteTypedValidator :: VoteValidatorScript
voteTypedValidator = voteTypedValidator' sampleValidatorParams

voteValidatorScriptHash :: ScriptHash
voteValidatorScriptHash = scriptHash voteTypedValidator

voteTypedValidator' :: ValidatorParams -> VoteValidatorScript
voteTypedValidator' config =
  mkTypedValidator
    (compiledVoteValidator `PlutusTx.applyCode` PlutusTx.liftCode config)

compiledVoteValidator ::
  PlutusTx.CompiledCode (ValidatorParams -> (BuiltinData -> BuiltinData -> BuiltinData -> ()))
compiledVoteValidator =
  $$(PlutusTx.compile [||mkUntypedValidator' . validateVote||])
