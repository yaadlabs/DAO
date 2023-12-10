{- |
Module      : Spec.Index.SampleData
Description : Index sample data for tests
-}
module Spec.Index.SampleData (
  validSampleIndexNftDatum,
) where

import LambdaBuffers.ApplicationTypes.Index (IndexNftDatum (IndexNftDatum, indexNftDatum'index))

-- | Valid index datum with the initial index set to zero
validSampleIndexNftDatum :: IndexNftDatum
validSampleIndexNftDatum = IndexNftDatum {indexNftDatum'index = 0}
