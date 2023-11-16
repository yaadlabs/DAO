module Spec.Vote.Utils (findVote) where

import Plutus.Model (Run)
import Plutus.V2.Ledger.Tx (TxOut, TxOutRef)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyVoteConfigNftSymbol, dummyVoteConfigNftTokenName)
import Triphut.Vote (VoteMinterDynamicConfigDatum)

findVote :: Run (TxOutRef, TxOut, VoteMinterDynamicConfigDatum)
findVote =
  findConfigUtxo
    alwaysSucceedTypedValidator1
    dummyVoteConfigNftSymbol
    dummyVoteConfigNftTokenName
