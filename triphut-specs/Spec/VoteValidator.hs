{- |
Module      : Spec.VoteValidator
Description : Tests for `validateVote` validator script
-}
module Spec.VoteValidator (spec) where

import Plutus.Model (
  MockConfig,
  adaValue,
  defaultBabbage,
  testNoErrors,
 )
import Spec.SpecUtils (amountOfAda, checkFails)
import Spec.Values (
  dummyConfigNftValue,
  dummyIndexConfigNftValue,
  dummyTallyConfigValue,
  dummyTallyValue,
  dummyVoteConfigNftValue,
  dummyVoteValue,
 )
import Spec.Vote.ContextValidator (
  validVoteValidatorTest,
 )
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat, (<>))

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Vote validator tests"
    [ positiveTest
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "Valid vote validator test" validVoteValidatorTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyVoteConfigNftValue
        , dummyTallyConfigValue
        , dummyVoteValue
        , dummyTallyValue
        ]
