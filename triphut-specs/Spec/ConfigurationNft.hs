module Spec.ConfigurationNft (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.ConfigurationNft.Script (mkNftMinterTooManyTokensTx, mkValidNftTx)
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
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config (adaValue 10_000_000)
    positiveTest = good "Configuration mint NFT (mkNftMinter) succeeds, positive test" mkValidNftTx
    negativeTest =
      bad "Configuration mint NFT (mkNftMinter) fails - more than 1 token in Tx" mkNftMinterTooManyTokensTx
    initialFunds = adaValue 10_000_000
