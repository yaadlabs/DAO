{- |
Module      : Spec.Tally
Description : Tests for `tallyNftPolicy` minting policy
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
    [positiveTest]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "valid test" validTallyConfigNftTest
    initialFunds = adaValue 10_000_000 <> dummyConfigNftValue <> dummyIndexConfigNftValue
