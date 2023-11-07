module Spec.ConfigurationNft (spec) where

import Spec.SpecUtils (checkFails)
import Spec.ConfigurationNft.Script (mkValidNftTx, mkNftMinterTooManyTokensTx)
import Test.Tasty (TestTree, testGroup)
import Plutus.Model 
  ( MockConfig
  , adaValue
  , defaultBabbage
  , testNoErrors
  )

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
