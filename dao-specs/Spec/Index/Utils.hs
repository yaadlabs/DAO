module Spec.Index.Utils where -- (findIndex) where

import Dao.Index (IndexNftDatum)
import Plutus.Model (Run)
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)

-- import Spec.Index.Script (indexNftTypedValidator)
import Spec.SpecUtils (findConfigUtxo)
import Spec.Values (dummyIndexConfigNftSymbol, dummyIndexConfigNftTokenName)

-- findIndex :: Run (TxOutRef, TxOut, IndexNftDatum)
-- findIndex =
--   findConfigUtxo
--     indexNftTypedValidator
--     dummyIndexConfigNftSymbol
--     dummyIndexConfigNftTokenName
