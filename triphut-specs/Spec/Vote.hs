{- |
Module      : Spec.Vote
Description : Tests for `voteNftPolicy` minting policy
-}
module Spec.Vote (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.SpecUtils (checkFails)
import Spec.Values (dummyConfigNftValue, dummyIndexConfigNftValue, dummyTallyValue, dummyVoteConfigNftValue)
import Spec.Vote.Context (
  validVoteConfigNftTest,
 )
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Vote NFT policy tests"
    [positiveTest]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "valid test" validVoteConfigNftTest
    initialFunds =
      mconcat
        [ adaValue 10_000_000
        , dummyConfigNftValue
        , dummyIndexConfigNftValue
        , dummyVoteConfigNftValue
        , dummyTallyValue
        ]
