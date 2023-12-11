module Spec.Configuration.Transactions (
  runInitConfig,
  runHighRelativeMajorityTotalVotesInitConfig,
) where

import Plutus.Model (Run)
import Spec.Configuration.Script (upgradeConfigNftTypedValidator)
import Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
 )
import Spec.SpecUtils (runInitReferenceScript)
import Spec.Values (dummyConfigNftValue)

runInitConfig :: Run ()
runInitConfig =
  runInitReferenceScript
    upgradeConfigNftTypedValidator
    sampleDynamicConfig
    dummyConfigNftValue

runHighRelativeMajorityTotalVotesInitConfig :: Run ()
runHighRelativeMajorityTotalVotesInitConfig =
  runInitReferenceScript
    upgradeConfigNftTypedValidator
    sampleHighRelativeMajorityHighTotalVotesDynamicConfig
    dummyConfigNftValue
