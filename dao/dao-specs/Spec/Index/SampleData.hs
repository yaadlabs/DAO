{- |
Module      : Spec.Index.SampleData
Description : Index sample data for tests
-}
module Spec.Index.SampleData (
  validSampleIndexDatum,
) where

import LambdaBuffers.ApplicationTypes.Index (IndexDatum (IndexDatum, indexDatum'index))

-- | Valid index datum with the initial index set to zero
validSampleIndexDatum :: IndexDatum
validSampleIndexDatum = IndexDatum {indexDatum'index = 0}
