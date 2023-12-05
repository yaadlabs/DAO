{- |
Module      : Spec.Treasury
Description : Tests for `validateTreasury` validator script
-}
module Spec.Treasury (spec) where

import Prelude (undefined)

spec = undefined

-- import Plutus.Model (
--   MockConfig,
--   defaultBabbage,
--   testNoErrors,
--  )
-- import Spec.SpecUtils (amountOfAda)
-- import Spec.Treasury.Context (
--   invalidNotEnoughVotesTripTreasuryTest,
--   validGeneralTreasuryTest,
--   validTripTreasuryTest,
--   validUpgradeTreasuryTest,
--  )
-- import Spec.Values (dummyConfigNftValue, dummyTallyValue, dummyTreasuryValue)
-- import Test.Tasty (TestTree, testGroup)
-- import Prelude (mconcat)
--
-- spec :: TestTree
-- spec = nftSpec defaultBabbage
--
-- nftSpec :: MockConfig -> TestTree
-- nftSpec config =
--   testGroup
--     "Treasury validator tests"
--     [ positiveTripTest
--     , positiveUpgradeTest
--     , positiveGeneralTest
--     , negativeTest1
--     ]
--   where
--     good = testNoErrors initialFunds config
--     positiveTripTest =
--       good
--         "Valid treasury test - trip proposal, should pass"
--         validTripTreasuryTest
--     positiveUpgradeTest =
--       good
--         "Valid treasury test - upgrade proposal, should pass"
--         validUpgradeTreasuryTest
--     positiveGeneralTest =
--       good
--         "Valid treasury test - general proposal, should pass"
--         validGeneralTreasuryTest
--
--     negativeTest1 =
--       good
--         ( mconcat
--             [ "Invalid trip treasury test - should fail with: "
--             , ""
--             ]
--         )
--         invalidNotEnoughVotesTripTreasuryTest
--
--     initialFunds =
--       mconcat
--         [ amountOfAda 20_000_000
--         , dummyConfigNftValue
--         , dummyTallyValue
--         , dummyTreasuryValue
--         ]
