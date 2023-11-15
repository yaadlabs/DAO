module Spec.ConfigurationNft.Transactions (runInitConfig) where

import Plutus.Model (Run)
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator)
import Spec.SampleData (sampleDynamicConfig)
import Spec.SpecUtils (minAda, runInitReferenceScript)
import Spec.Values (dummyConfigNftValue)
import Prelude ((<>))

runInitConfig :: Run ()
runInitConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator
    sampleDynamicConfig
    (dummyConfigNftValue <> minAda)
