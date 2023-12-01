{- |
Module      : Spec.Index.SampleData
Description : Index sample data for tests
-}
module Spec.Index.SampleData (
  validSampleIndexNftDatum,
) where

import Dao.Index (
  IndexNftDatum (IndexNftDatum, indIndex),
 )
import Plutus.V1.Ledger.Value (adaSymbol, adaToken)

-- | Valid index datum with the initial index set to zero
validSampleIndexNftDatum :: IndexNftDatum
validSampleIndexNftDatum = IndexNftDatum {indIndex = 0}
