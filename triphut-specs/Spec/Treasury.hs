{- |
Module      : Spec.Treasury
Description : Tests for `validateTreasury` validator script
-}
module Spec.Treasury (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.SpecUtils (amountOfAda, checkFails)
import Spec.Treasury.Context (
  validGeneralTreasuryTest,
  validTripTreasuryTest,
  validUpgradeTreasuryTest,
 )
import Spec.Values (dummyConfigNftValue, dummyTallyValue, dummyTreasuryValue)
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Treasury validator tests"
    [ positiveTripTest
    , positiveUpgradeTest
    , positiveGeneralTest
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTripTest =
      good
        "Valid treasury test - trip proposal, should pass"
        validTripTreasuryTest
    positiveUpgradeTest =
      good
        "Valid treasury test - upgrade proposal, should pass"
        validUpgradeTreasuryTest
    positiveGeneralTest =
      good
        "Valid treasury test - general proposal, should pass"
        validGeneralTreasuryTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyConfigNftValue
        , dummyTallyValue
        , dummyTreasuryValue
        ]