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
  invalidNotSignedByOwnerVoteValidatorCancelRedeemerTest,
  invalidVoteStillInTallyPeriodTest,
  invalidVoteValidatorNoConfigInRefInputsTest,
  invalidVoteValidatorNoTallyConfigInRefInputsTest,
  invalidVoteValidatorNoTallyInInputsTest,
  invalidVoteValidatorNoVoteInInputsTest,
  validVoteValidatorCancelRedeemerTest,
  validVoteValidatorCountRedeemerTest,
 )
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat, (<>))

spec :: TestTree
spec = nftSpec defaultBabbage

nftSpec :: MockConfig -> TestTree
nftSpec config =
  testGroup
    "Vote validator tests"
    [ positiveCountRedeemerTest
    , negativeCountRedeemerTest1
    , negativeCountRedeemerTest2
    , negativeCountRedeemerTest3
    , negativeCountRedeemerTest4
    , negativeCountRedeemerTest5
    , positiveCancelRedeemerTest
    , negativeCancelRedeemerTest1
    ]
  where
    -- Count redeemer tests
    positiveCountRedeemerTest = good "Valid vote validator, Cancel redeemer, test" validVoteValidatorCountRedeemerTest
    negativeCountRedeemerTest1 =
      bad
        ( mconcat
            [ "No config in the reference inputs, should fail with: "
            , "[Should be exactly one config NFT in the reference inputs. None found.]"
            ]
        )
        invalidVoteValidatorNoConfigInRefInputsTest
    negativeCountRedeemerTest2 =
      bad
        ( mconcat
            [ "No tally validator in inputs, should fail with a balancing error. "
            , "Corresponding script error is: "
            , "Missing Tally Validator input"
            ]
        )
        invalidVoteValidatorNoTallyInInputsTest
    negativeCountRedeemerTest3 =
      bad
        "No tally validator in inputs, should fail with balancing error: "
        invalidVoteValidatorNoTallyConfigInRefInputsTest
    negativeCountRedeemerTest4 =
      bad
        "No vote validator in inputs, should fail with a balancing error. "
        invalidVoteValidatorNoVoteInInputsTest
    negativeCountRedeemerTest5 =
      bad
        ( mconcat
            [ "Tally period not over, should fail with: "
            , "[Tally is active]"
            ]
        )
        invalidVoteStillInTallyPeriodTest

    -- Cancel redeemer tests
    positiveCancelRedeemerTest = good "Valid vote validator, Cancel redeemer, test" validVoteValidatorCancelRedeemerTest
    negativeCancelRedeemerTest1 =
      bad
        ( mconcat
            [ "Tally period not over, should fail with: "
            , "[Transaction should be signed by the vote owner]"
            ]
        )
        invalidNotSignedByOwnerVoteValidatorCancelRedeemerTest

    initialFunds =
      mconcat
        [ amountOfAda 20_000_000
        , dummyVoteConfigNftValue
        , dummyTallyConfigValue
        , dummyVoteValue
        , dummyTallyValue
        ]
    good = testNoErrors initialFunds config
    bad = checkFails config initialFunds
