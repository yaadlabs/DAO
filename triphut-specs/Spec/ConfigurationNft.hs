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
import Prelude ((<>))

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
        ( "Configuration mint NFT (mkNftMinter) fails with script errors: "
            <> "[Should be exactly one token, Impossible. No valid minted output.]"
            <> " - due to more than 1 of NFT being minted in Tx"
        )
        invalidConfigNftTooManyTokensMintedTest
    negativeTest1 =
      bad
        ( "Configuration mint NFT (mkNftMinter) fails with script error: "
            <> "(Missing datum hash or datum) - no Datum paid to validator script"
        )
        invalidConfigNftNoDatumPaidToScriptTest
    initialFunds = adaValue 10_000_000
