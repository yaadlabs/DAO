{- |
Module      : Spec.Tally
Description : Tests for `tallyNftPolicy` minting policy.
  Also tests the `validateIndex` validator in the same transaction.
-}
module Spec.Tally (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.SpecUtils (checkFails)
import Spec.Tally.Context (
  invalidDoesNotSpendIndexConfigNftTest,
  invalidIndexNotIncrementedConfigNftTest,
  invalidMoreThanOneTokenMintedTallyConfigNftTest,
  invalidNoConfigInRefInputsConfigNftTest,
  invalidWrongTokenNameTallyConfigNftTest,
  validTallyConfigNftTest,
 )
import Spec.Values (dummyConfigNftValue, dummyIndexConfigNftValue)
import Test.Tasty (TestTree, testGroup)
import Prelude ((<>))

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Tally NFT policy tests"
    [ positiveTest
    , negativeTest1
    , negativeTest2
    , negativeTest3
    , negativeTest4
    , negativeTest5
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "Valid tally validator test, should pass" validTallyConfigNftTest
    negativeTest1 =
      bad
        ( "Wrong token name, should fail with: "
            <> "[Should be exactly one valid Tally NFT output]"
        )
        invalidWrongTokenNameTallyConfigNftTest
    negativeTest2 =
      bad
        ( "More than one token minted, should fail with: "
            <> "[Should be exactly one valid Tally NFT output]"
        )
        invalidMoreThanOneTokenMintedTallyConfigNftTest
    negativeTest3 =
      bad
        ( "Index field of output IndexNftDatum not incremented, should fail with: "
            <> "[output datum is not incremented]"
        )
        invalidIndexNotIncrementedConfigNftTest
    negativeTest4 =
      bad
        ( "Config not in ref inputs, should fail with: "
            <> "[Should be exactly one valid config in the reference inputs]"
        )
        invalidNoConfigInRefInputsConfigNftTest
    negativeTest5 =
      bad
        "Doesn't spend index, should fail with balancing error"
        invalidDoesNotSpendIndexConfigNftTest
    initialFunds = adaValue 10_000_000 <> dummyConfigNftValue <> dummyIndexConfigNftValue
