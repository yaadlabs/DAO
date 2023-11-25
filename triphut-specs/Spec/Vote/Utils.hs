module Spec.Vote.Utils (
  findVoteMinterConfig,
  findVote,
) where

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
import Triphut.Vote (VoteDatum, VoteMinterDynamicConfigDatum)

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
