module Spec.Vote.Utils (
  findVote,
) where

import Dao.Vote (VoteDatum)
import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (
  dummyVoteSymbol,
  dummyVoteTokenName,
 )
import Spec.Vote.Script (voteTypedValidator)

findVote :: Run (TxOutRef, TxOut, VoteDatum)
findVote =
  findConfigUtxo
    voteTypedValidator
    dummyVoteSymbol
    dummyVoteTokenName
