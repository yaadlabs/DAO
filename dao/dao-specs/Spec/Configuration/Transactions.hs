module Spec.Configuration.Transactions (
  runInitConfig,
  runHighRelativeMajorityTotalVotesInitConfig,
  runInitTreasuryTestConfig,
  runInitHighThresholdTreasuryTestConfig,
) where

import Plutus.Model (Run)
import Spec.Configuration.Script (upgradeConfigNftTypedValidator)
import Spec.SampleData (
  sampleDynamicConfig,
  sampleHighRelativeMajorityHighTotalVotesDynamicConfig,
  sampleHighThresholdTreasuryTestDynamicConfig,
  sampleTreasuryTestDynamicConfig,
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

-- Special config for Treasury tests that uses alwaysSucceed for tally burning (ID-501)
runInitTreasuryTestConfig :: Run ()
runInitTreasuryTestConfig =
  runInitReferenceScript
    upgradeConfigNftTypedValidator
    sampleTreasuryTestDynamicConfig
    dummyConfigNftValue

-- Special config for negative Treasury tests (high threshold + alwaysSucceed tally)
runInitHighThresholdTreasuryTestConfig :: Run ()
runInitHighThresholdTreasuryTestConfig =
  runInitReferenceScript
    upgradeConfigNftTypedValidator
    sampleHighThresholdTreasuryTestDynamicConfig
    dummyConfigNftValue
