{- |
Module      : Spec.Upgrade
Description : Tests for `validateConfiguration` validator script
-}
module Spec.Upgrade (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.SpecUtils (amountOfAda, checkFails)
import Spec.Upgrade.Context (
  validUpgradeTest,
 )
import Spec.Values (dummyConfigNftValue, dummyTallyValue)
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Upgrade validator tests"
    [ positiveTest
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "Valid upgrade proposal test, should pass" validUpgradeTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyConfigNftValue
        , dummyTallyValue
        ]