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
import Spec.ConfigurationNft.Context (
  invalidConfigNftNoDatumPaidToScriptTest,
  invalidConfigNftTooManyTokensMintedTest,
  invalidConfigNftWrongTokenNameTest,
  validConfigNftTest,
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
    []
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    initialFunds = adaValue 10_000_000
