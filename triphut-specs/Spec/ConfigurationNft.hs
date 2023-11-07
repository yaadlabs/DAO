module Spec.ConfigurationNft (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.ConfigurationNft.Context (
  invalidConfigNftNoDatumPaidToScriptTest,
  invalidConfigNftTooManyTokensMintedTest,
  validConfigNftTest,
 )
import Spec.SpecUtils (checkFails)
import Test.Tasty (TestTree, testGroup)

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Configuration NFT policy tests"
    [ positiveTest
    , negativeTest
    , negativeTest1
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config (adaValue 10_000_000)
    positiveTest = good "Configuration mint NFT (mkNftMinter) succeeds, positive test" validConfigNftTest
    negativeTest =
      bad
        "Configuration mint NFT (mkNftMinter) fails - more than 1 token in Tx"
        invalidConfigNftTooManyTokensMintedTest
    negativeTest1 =
      bad
        "Configuration mint NFT (mkNftMinter) fails - no Datum paid to validator script"
        invalidConfigNftNoDatumPaidToScriptTest
    initialFunds = adaValue 10_000_000
