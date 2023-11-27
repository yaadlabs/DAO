module Spec.Vote.Utils (findVoteConfig, findVote) where

import Dao.Vote (VoteDatum, VoteMinterDynamicConfigDatum)
import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (
  dummyVoteConfigNftSymbol,
  dummyVoteConfigNftTokenName,
  dummyVoteSymbol,
  dummyVoteTokenName,
 )
import Spec.Vote.Script (voteTypedValidator)

findVoteConfig :: Run (TxOutRef, TxOut, VoteMinterDynamicConfigDatum)
findVoteConfig =
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
