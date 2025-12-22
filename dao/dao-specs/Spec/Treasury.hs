{- |
Module      : Spec.Treasury
Description : Tests for `validateTreasury` validator script
-}
module Spec.Treasury (spec) where

import Plutus.Model (
  MockConfig,
  defaultBabbageV2,
  testNoErrors,
 )
import PlutusLedgerApi.V1.Value (singleton)
import Spec.AlwaysSucceed.Script (alwaysSucceedCurrencySymbol)
import Spec.SpecUtils (amountOfAda)
import Spec.Treasury.Context (
  invalidNotEnoughVotesTripTreasuryTest,
  validGeneralTreasuryTest,
  validTripTreasuryTest,
  validUpgradeTreasuryTest,
 )
import Spec.Values (dummyConfigNftValue, dummyIndexConfigNftValue, dummyTallyTokenName, dummyTreasuryValue, dummyVoteNFTValue)
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)

spec :: TestTree
spec = nftSpec defaultBabbageV2

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Treasury validator tests"
    [ positiveTripTest
    , positiveUpgradeTest
    , positiveGeneralTest
    , negativeTest1
    ]
  where
    good = testNoErrors initialFunds config
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

    negativeTest1 =
      good
        ( mconcat
            [ "Invalid trip treasury test - should fail with: "
            , ""
            ]
        )
        invalidNotEnoughVotesTripTreasuryTest

    initialFunds =
      mconcat
        [ amountOfAda 50_000_000 -- Need more for runInitTreasuryWithFunds (4M) + disbursements
        , dummyConfigNftValue
        , dummyIndexConfigNftValue
        , singleton alwaysSucceedCurrencySymbol dummyTallyTokenName 1 -- AlwaysSucceed tally (for burning per ID-501)
        , dummyTreasuryValue -- Real treasury value (no separate policy tests)
        , dummyVoteNFTValue
        ]
