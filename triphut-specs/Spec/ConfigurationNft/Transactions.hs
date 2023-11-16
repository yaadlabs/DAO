module Spec.ConfigurationNft.Transactions (
  runInitConfig,
  runHighRelativeMajorityTotalVotesInitConfig,
) where

import Plutus.Model (Run)
import Spec.ConfigurationNft.Script (upgradeConfigNftTypedValidator)
import Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
 )
import Spec.SpecUtils (runInitReferenceScript)
import Spec.Values (dummyConfigNftValue)
import Prelude ((<>))

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
