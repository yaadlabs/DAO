{- |
Module      : Spec.Vote
Description : Tests for `voteNftPolicy` minting policy
-}
module Spec.Vote (spec) where

import Prelude (undefined)

spec = undefined

-- import Plutus.Model (
--   MockConfig,
--   adaValue,
--   defaultBabbage,
--   testNoErrors,
--  )
-- import Spec.SpecUtils (checkFails)
-- import Spec.Values (dummyConfigNftValue, dummyIndexConfigNftValue, dummyTallyValue)
-- import Spec.Vote.Context (
--   invalidMoreThanOneTokenVoteConfigNftTest,
--   invalidNoConfigInRefInputsVoteConfigNftTest,
--   invalidProposalEndTimeNotAfterValidityRangeVoteConfigNftTest,
--   validVoteConfigNftTest,
--  )
-- import Test.Tasty (TestTree, testGroup)
-- import Prelude (mconcat, (<>))
--
-- spec :: TestTree
-- spec = nftSpec defaultBabbage
--
-- nftSpec :: MockConfig -> TestTree
-- nftSpec config =
--   testGroup
--     "Vote NFT policy tests"
--     [ positiveTest
--     , negativeTest1
--     , negativeTest2
--     , negativeTest3
--     ]
--   where
--     good = testNoErrors initialFunds config
--     bad = checkFails config initialFunds
--     positiveTest = good "Valid vote policy test" validVoteConfigNftTest
--     negativeTest1 =
--       bad
--         ( "More than one token minted, should fail with: "
--             <> "[Should be exactly one token, Vote NFT: Token count should be exactly one]"
--         )
--         invalidMoreThanOneTokenVoteConfigNftTest
--     negativeTest2 =
--       bad
--         ( "Config not in ref inputs, should fail with: "
--             <> "[Should be exactly one valid config in the reference inputs]"
--         )
--         invalidNoConfigInRefInputsVoteConfigNftTest
--     negativeTest3 =
--       bad
--         ( "Proposal end time not after validity range, should fail with: "
--             <> "[Proposal has expired] error"
--         )
--         invalidProposalEndTimeNotAfterValidityRangeVoteConfigNftTest
--
--     initialFunds =
--       mconcat
--         [ adaValue 10_000_000
--         , dummyConfigNftValue
--         , dummyIndexConfigNftValue
--         , dummyTallyValue
--         ]
