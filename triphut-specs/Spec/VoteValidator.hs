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
  invalidVoteValidatorNoConfigInRefInputsTest,
  invalidVoteValidatorNoTallyConfigInRefInputsTest,
  invalidVoteValidatorNoTallyInInputsTest,
  invalidVoteValidatorNoVoteInInputsTest,
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
    , negativeTest1
    , negativeTest2
    , negativeTest3
    , negativeTest4
    ]
  where
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
    positiveTest = good "Valid vote validator test" validVoteValidatorTest
    negativeTest1 =
      bad
        ( mconcat
            [ "No config in the reference inputs, should fail with: "
            , "[Should be exactly one config NFT in the reference inputs. None found.]"
            ]
        )
        invalidVoteValidatorNoConfigInRefInputsTest
    negativeTest2 =
      bad
        ( mconcat
            [ "No tally validator in inputs, should fail with a balancing error. "
            , "Corresponding script error is: "
            , "Missing Tally Validator input"
            ]
        )
        invalidVoteValidatorNoTallyInInputsTest
    negativeTest3 =
      bad
        "No tally validator in inputs, should fail with balancing error: "
        invalidVoteValidatorNoTallyConfigInRefInputsTest
    negativeTest4 =
      bad
        "No vote validator in inputs, should fail with a balancing error. "
        invalidVoteValidatorNoVoteInInputsTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyVoteConfigNftValue
        , dummyTallyConfigValue
        , dummyVoteValue
        , dummyTallyValue
        ]
