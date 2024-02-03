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

import Dao.ScriptArgument (ConfigurationValidatorConfig (ConfigurationValidatorConfig))
import Dao.Shared (mkUntypedValidator')
import Dao.Vote.Script (validateVote, wrappedPolicy)
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
import PlutusTx.Prelude (BuiltinData, ($), (.))
import Spec.Configuration.SampleData (sampleConfigValidatorConfig)

-- Policy script and info
type VoteMintingPolicy = TypedPolicy VoteMinterActionRedeemer

voteTypedMintingPolicy :: ConfigurationValidatorConfig -> VoteMintingPolicy
voteTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||wrappedPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

voteCurrencySymbol :: ConfigurationValidatorConfig -> CurrencySymbol
voteCurrencySymbol = scriptCurrencySymbol . voteTypedMintingPolicy

voteValue :: ConfigurationValidatorConfig -> Value
voteValue voteCfg@(ConfigurationValidatorConfig _ tokenName) = singleton (voteCurrencySymbol voteCfg) tokenName 1

-- Validator script and info
type VoteValidatorScript = TypedValidator VoteDatum VoteActionRedeemer

voteTypedValidator :: VoteValidatorScript
voteTypedValidator = voteTypedValidator' sampleConfigValidatorConfig

voteValidatorScriptHash :: ScriptHash
voteValidatorScriptHash = scriptHash voteTypedValidator

voteTypedValidator' :: ConfigurationValidatorConfig -> VoteValidatorScript
voteTypedValidator' config =
  mkTypedValidator
    (compiledVoteValidator `PlutusTx.applyCode` PlutusTx.liftCode config)

compiledVoteValidator ::
  PlutusTx.CompiledCode (ConfigurationValidatorConfig -> (BuiltinData -> BuiltinData -> BuiltinData -> ()))
compiledVoteValidator =
  $$(PlutusTx.compile [||mkUntypedValidator' . validateVote||])
