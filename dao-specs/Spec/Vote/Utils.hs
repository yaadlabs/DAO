module Spec.Vote.Utils (
  findVoteConfig,
  findVoteMinterConfig,
  findVote,
) where

import Dao.Vote (VoteDatum, VoteDynamicConfigDatum, VoteMinterDynamicConfigDatum)
import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1, alwaysSucceedTypedValidator3)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (
  dummyVoteConfigNftSymbol,
  dummyVoteConfigNftTokenName,
  dummyVoteSymbol,
  dummyVoteTokenName,
 )
import Spec.Vote.Script (voteTypedValidator)

findVoteConfig :: Run (TxOutRef, TxOut, VoteDynamicConfigDatum)
findVoteConfig =
  findConfigUtxo
    alwaysSucceedTypedValidator3
    dummyVoteConfigNftSymbol
    dummyVoteConfigNftTokenName

findVoteMinterConfig :: Run (TxOutRef, TxOut, VoteMinterDynamicConfigDatum)
findVoteMinterConfig =
  findConfigUtxo
    alwaysSucceedTypedValidator1
    dummyVoteConfigNftSymbol
    dummyVoteConfigNftTokenName

findVote :: Run (TxOutRef, TxOut, VoteDatum)
findVote =
  findConfigUtxo
    voteTypedValidator
    dummyVoteSymbol
    dummyVoteTokenName
