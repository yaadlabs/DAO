{- |
Module      : Spec.Index
Description : Tests for `indexNftPolicy` minting policy
-}
module Spec.Index (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.Index.Context (
  invalidMoreThanOneTokenMintedIndexConfigNftTest,
  invalidNoDatumSentToValidadtorIndexConfigNftTest,
  validIndexConfigNftTest,
 )
import Spec.SpecUtils (checkFails)
import Test.Tasty (TestTree, testGroup)
import Prelude ((<>))

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Index NFT policy tests"
    [ positiveTest
    , negativeTest1
    , negativeTest2
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "valid test" validIndexConfigNftTest
    negativeTest1 =
      bad
        ( "More than one token minted, should fail with: "
            <> "[Should be exactly one token, Should be exactly one valid minted output.]"
        )
        invalidMoreThanOneTokenMintedIndexConfigNftTest
    negativeTest2 =
      bad
        ( "No datum sent, should fail with: "
            <> "[Missing datum hash or datum]"
        )
        invalidNoDatumSentToValidadtorIndexConfigNftTest

    initialFunds = adaValue 10_000_000
