module Spec.Vote.Script (
  voteTypedMintingPolicy,
  voteMintingPolicy,
  voteCurrencySymbol,
  voteValue,
)
where

import Plutus.Model.V2 (
  TypedPolicy,
  mkTypedPolicy,
  scriptCurrencySymbol,
 )
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  mkMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Triphut.Vote (VoteMinterConfig (VoteMinterConfig), wrappedPolicy)

voteTypedMintingPolicy :: VoteMinterConfig -> TypedPolicy ()
voteTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

voteMintingPolicy :: VoteMinterConfig -> MintingPolicy
voteMintingPolicy config =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

voteCurrencySymbol :: VoteMinterConfig -> CurrencySymbol
voteCurrencySymbol = scriptCurrencySymbol . voteTypedMintingPolicy

voteValue :: VoteMinterConfig -> Value
voteValue voteCfg@(VoteMinterConfig _ tokenName) = singleton (voteCurrencySymbol voteCfg) tokenName 1
