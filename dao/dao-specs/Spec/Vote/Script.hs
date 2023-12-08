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

import Dao.ConfigurationNft (ConfigurationValidatorConfig (ConfigurationValidatorConfig))
import Dao.Vote (
  VoteActionRedeemer,
  VoteDatum,
  VoteMinterActionRedeemer,
 )
import Dao.Vote.Script (voteValidatorCompiledCode, wrappedPolicy)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
  scriptHash,
 )
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')

-- Policy script and info
type VoteMintingPolicy = TypedPolicy VoteMinterActionRedeemer

voteTypedMintingPolicy :: ConfigurationValidatorConfig -> VoteMintingPolicy
voteTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

voteCurrencySymbol :: ConfigurationValidatorConfig -> CurrencySymbol
voteCurrencySymbol = scriptCurrencySymbol . voteTypedMintingPolicy

voteValue :: ConfigurationValidatorConfig -> Value
voteValue voteCfg@(ConfigurationValidatorConfig _ tokenName) = singleton (voteCurrencySymbol voteCfg) tokenName 1

-- Validator script and info
type VoteValidatorScript = TypedValidator VoteDatum VoteActionRedeemer

voteTypedValidator :: VoteValidatorScript
voteTypedValidator = mkTypedValidator' voteValidatorCompiledCode sampleConfigValidatorConfig

voteValidatorScriptHash :: ScriptHash
voteValidatorScriptHash = scriptHash voteTypedValidator
