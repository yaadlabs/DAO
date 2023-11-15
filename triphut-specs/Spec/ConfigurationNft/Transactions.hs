module Spec.ConfigurationNft.Transactions (runInitConfig) where

import Plutus.Model (Run)
import Spec.ConfigurationNft.Script (upgradeConfigNftTypedValidator)
import Spec.SampleData (sampleDynamicConfig)
import Spec.SpecUtils (runInitReferenceScript)
import Spec.Values (dummyConfigNftValue)
import Prelude ((<>))

runInitConfig :: Run ()
runInitConfig =
  runInitReferenceScript
    upgradeConfigNftTypedValidator
    sampleDynamicConfig
    dummyConfigNftValue
