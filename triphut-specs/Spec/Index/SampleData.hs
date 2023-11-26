{- |
Module      : Spec.Index.SampleData
Description : Index sample data for tests
-}
module Spec.Index.SampleData (
  validSampleIndexNftDatum,
) where

import Triphut.Index (
  IndexNftDatum (IndexNftDatum, indIndex),
 )

-- | Valid index datum with the initial index set to zero
validSampleIndexNftDatum :: IndexNftDatum
validSampleIndexNftDatum = IndexNftDatum {indIndex = 0}
