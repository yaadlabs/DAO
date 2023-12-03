module Spec.Vote.Script (
  VoteValidatorScript,
  VoteMintingPolicy,
  voteTypedValidator,
  voteValidatorHash',
  voteTypedMintingPolicy,
  voteMintingPolicy,
  voteCurrencySymbol,
  voteValue,
)
where

import Dao.ConfigurationNft (ConfigurationValidatorConfig (ConfigurationValidatorConfig))
import Dao.Vote (
  VoteActionRedeemer,
  VoteDatum,
  VoteMinterActionRedeemer,
 )
import Dao.Vote.Script (voteValidator, voteValidatorHash, wrappedPolicy)
import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
 )
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  ValidatorHash,
  mkMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')

-- Policy script and info
type VoteMintingPolicy = TypedPolicy VoteMinterActionRedeemer

voteTypedMintingPolicy :: ConfigurationValidatorConfig -> VoteMintingPolicy
voteTypedMintingPolicy config =
  mkTypedPolicy
    $ $$(PlutusTx.compile [||\c -> wrappedPolicy c||])
    `PlutusTx.applyCode` PlutusTx.liftCode config

voteMintingPolicy :: ConfigurationValidatorConfig -> MintingPolicy
voteMintingPolicy config =
  mkMintingPolicyScript
    $ $$(PlutusTx.compile [||\c -> wrappedPolicy c||])
    `PlutusTx.applyCode` PlutusTx.liftCode config

voteCurrencySymbol :: ConfigurationValidatorConfig -> CurrencySymbol
voteCurrencySymbol = scriptCurrencySymbol . voteTypedMintingPolicy

voteValue :: ConfigurationValidatorConfig -> Value
voteValue voteCfg@(ConfigurationValidatorConfig _ tokenName) = singleton (voteCurrencySymbol voteCfg) tokenName 1

-- Validator script and info
type VoteValidatorScript = TypedValidator VoteDatum VoteActionRedeemer

voteTypedValidator :: VoteValidatorScript
voteTypedValidator = mkTypedValidator' sampleConfigValidatorConfig voteValidator

voteValidatorHash' :: ValidatorHash
voteValidatorHash' = voteValidatorHash sampleConfigValidatorConfig
