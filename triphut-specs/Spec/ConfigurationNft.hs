{- |
Module      : Spec.ConfigurationNft
Description : Tests for `configurationNftPolicy`
-}
module Spec.ConfigurationNft (spec) where

import Control.Monad (void)
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
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.SpecUtils (checkFails)
import Spec.Values (dummyConfigNftValue)
import Test.Tasty (TestTree, testGroup)
import Prelude ((<>))

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Configuration NFT policy tests"
    [ positiveTest
    , positiveTest1
    , negativeTest
    , negativeTest1
    , negativeTest2
    ]
  where
    good = testNoErrors initialFunds config
    goodWithToken = testNoErrors (initialFunds <> dummyConfigNftValue) config
    bad = checkFails config initialFunds
    positiveTest =
      good
        "Configuration mint NFT (mkNftMinter) succeeds, positive test"
        validConfigNftTest
    positiveTest1 =
      goodWithToken
        "Create config NFT, positive test"
        (void runInitConfig)
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
    negativeTest2 =
      bad
        ( "Configuration mint NFT (mkNftMinter) fails with script error: "
            <> "[Incorrect token name provided, Only one valid token minted, PT5]"
            <> " - due to wrong token name in config"
        )
        invalidConfigNftWrongTokenNameTest
    initialFunds = adaValue 10_000_000
