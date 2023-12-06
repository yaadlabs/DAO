{- |
Module      : Spec.Upgrade
Description : Tests for `validateConfiguration` validator script
-}
module Spec.Upgrade (spec) where

import Plutus.Model (
  MockConfig,
  defaultBabbageV2,
  testNoErrors,
 )
import Spec.SpecUtils (amountOfAda, checkFails)
import Spec.Upgrade.Context (
  invalidUpgradeNoConfigInputTest,
  invalidUpgradeNoTallyReferenceTest,
  invalidUpgradeNoUpgradeTokenMintedTest,
  invalidUpgradeNotEnoughVotesTest,
  validUpgradeTest,
 )
import Spec.Values (dummyConfigNftValue, dummyTallyValue)
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)

spec :: TestTree
spec = nftSpec defaultBabbageV2

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Upgrade validator tests"
    [ positiveTest
    , negativeTest1
    , negativeTest2
    , negativeTest3
    , negativeTest4
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "Valid upgrade proposal test, should pass" validUpgradeTest
    negativeTest1 =
      bad
        ( mconcat
            [ "Invalid upgrade proposal test - should fail with: "
            , "Should be exactly one tally NFT in the reference inputs. None found."
            ]
        )
        invalidUpgradeNoTallyReferenceTest
    negativeTest2 =
      bad
        ( mconcat
            [ "Invalid upgrade proposal test - should fail with: "
            , "Balancing error - corresponding script error is: "
            , "Should be exactly one configuration NFT in the inputs"
            ]
        )
        invalidUpgradeNoConfigInputTest
    negativeTest3 =
      bad
        ( mconcat
            [ "Invalid upgrade proposal test - should fail with: "
            , "Balancing error - corresponding script error is: "
            , "Should be exactly one upgrade token minted"
            ]
        )
        invalidUpgradeNoUpgradeTokenMintedTest
    negativeTest4 =
      bad
        ( mconcat
            [ "Invalid upgrade proposal test - should fail with: "
            , "[relative majority is too low"
            , "The proposal doesn't have enough votes]"
            ]
        )
        invalidUpgradeNotEnoughVotesTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyConfigNftValue
        , dummyTallyValue
        ]
