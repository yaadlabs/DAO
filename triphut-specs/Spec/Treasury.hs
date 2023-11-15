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
  validTreasuryTest,
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
    [ positiveTest
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "Valid treasury test, should pass" validTreasuryTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyConfigNftValue
        , dummyTallyValue
        , dummyTreasuryValue
        ]
