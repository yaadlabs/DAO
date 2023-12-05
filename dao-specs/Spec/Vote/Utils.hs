module Spec.Vote.Utils where -- (
--   findVote,
-- ) where

-- import Dao.Vote (VoteDatum)
-- import Plutus.Model (Run)
-- import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
-- import Spec.SpecUtils (findConfigUtxo)
-- import Spec.Values (
--   dummyVoteSymbol,
--   dummyVoteTokenName,
--  )
-- import Spec.Vote.Script (voteTypedValidator)
--
-- findVote :: Run (TxOutRef, TxOut, VoteDatum)
-- findVote =
--   findConfigUtxo
--     voteTypedValidator
--     dummyVoteSymbol
--     dummyVoteTokenName
