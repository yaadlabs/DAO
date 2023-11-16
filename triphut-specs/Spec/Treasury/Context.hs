{- |
Module      : Spec.Treasury.Context
Description : Treasury validator unit tests
-}
module Spec.Treasury.Context (
  validTripTreasuryTest,
  validUpgradeTreasuryTest,
  validGeneralTreasuryTest,
) where

import Control.Monad (void)
import Plutus.Model (
  Run,
  adaValue,
  currentTime,
  mintValue,
  newUser,
  spend,
  spendScript,
  submitTx,
  userSpend,
  validateIn,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  logInfo,
  payToKey,
  payToScript,
  refInputInline,
 )
import Plutus.V1.Ledger.Interval (from)
import Plutus.V1.Ledger.Value (Value, adaToken, singleton)
import PlutusTx.Prelude (($))
import Spec.Addresses (
  dummyGeneralPaymentAddress,
  dummyTravelerPaymentAddress,
 )
import Spec.AlwaysSucceed.Script (
  alwaysSucceedCurrencySymbol,
  alwaysSucceedTypedMintingPolicy,
 )
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.Transactions (
  runInitGeneralTallyWithEndTimeInFuture,
  runInitTripTallyWithEndTimeInFuture,
  runInitUpgradeTallyWithEndTimeInPast,
  runInitUpgradeWithVotesWithEndTimeInFutureTallyStateDatum,
 )
import Spec.Tally.Utils (findTally)
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Treasury.Transactions (runInitTreasury)
import Spec.Treasury.Utils (findTreasury)
import Spec.Values (dummyTreasuryValue)
import Triphut.Vote (VoteActionRedeemer (Count))
import Prelude (mconcat, pure, show, (*), (+), (<>))

-- Positive test for when the proposal is an Trip proposal
validTripTreasuryTest :: Run ()
validTripTreasuryTest = do
  runInitConfig
  runInitTripTallyWithEndTimeInFuture
  runInitTreasury

  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (treasuryOutRef, _, _) <- findTreasury

  user <- newUser $ amountOfAda 9_000_000
  spend1 <- spend user $ amountOfAda 6_000_000
  spend2 <- spend user $ amountOfAda 6_000_000
  spend3 <- spend user $ amountOfAda 8_000_002

  let baseTx =
        mconcat
          [ spendScript treasuryTypedValidator treasuryOutRef () ()
          , refInputInline configOutRef
          , refInputInline tallyOutRef
          , userSpend spend1
          , userSpend spend2
          ]

      payToTreasuryValidator =
        payToScript
          treasuryTypedValidator
          (InlineDatum ())
          (amountOfAda 4_000_000 <> dummyTreasuryValue)

      -- Need to pay something to the traveller's payment address provided
      payToTravelerAddress =
        mconcat
          [ payToKey
              dummyTravelerPaymentAddress
              (adaValue 2)
          , userSpend spend3
          ]

      combinedTxs = baseTx <> payToTreasuryValidator <> payToTravelerAddress

  submitTx user combinedTxs

-- Positive test for when the proposal is an Upgrade proposal
validUpgradeTreasuryTest :: Run ()
validUpgradeTreasuryTest = do
  runInitConfig
  runInitUpgradeTallyWithEndTimeInPast
  runInitTreasury

  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (treasuryOutRef, _, _) <- findTreasury

  user <- newUser $ amountOfAda 8_000_000
  spend1 <- spend user $ amountOfAda 4_000_000
  spend2 <- spend user $ amountOfAda 2_000_002

  theTimeNow <- currentTime

  let
    upgradeToken :: Value
    upgradeToken = singleton alwaysSucceedCurrencySymbol adaToken 1

    baseTx =
      mconcat
        [ spendScript treasuryTypedValidator treasuryOutRef () ()
        , mintValue alwaysSucceedTypedMintingPolicy () upgradeToken
        , refInputInline configOutRef
        , refInputInline tallyOutRef
        , userSpend spend1
        , userSpend spend2
        ]

    payToTreasuryValidator =
      payToScript
        treasuryTypedValidator
        (InlineDatum ())
        (adaValue 2 <> dummyTreasuryValue)

    -- Pay it to the user, just for balancing the tx for now
    -- Not sure what is meant to happen with this token after minting it here
    payUpgradeTokenToUser = payToKey user upgradeToken

    combinedTxs = baseTx <> payToTreasuryValidator <> payUpgradeTokenToUser

  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx

-- Positive test for when the proposal is a General proposal
validGeneralTreasuryTest :: Run ()
validGeneralTreasuryTest = do
  runInitConfig
  runInitGeneralTallyWithEndTimeInFuture
  runInitTreasury

  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (treasuryOutRef, _, _) <- findTreasury

  user <- newUser $ amountOfAda 9_000_000
  spend1 <- spend user $ amountOfAda 6_000_000
  spend2 <- spend user $ amountOfAda 6_000_000
  spend3 <- spend user $ amountOfAda 8_000_002

  let baseTx =
        mconcat
          [ spendScript treasuryTypedValidator treasuryOutRef () ()
          , refInputInline configOutRef
          , refInputInline tallyOutRef
          , userSpend spend1
          , userSpend spend2
          ]

      payToTreasuryValidator =
        payToScript
          treasuryTypedValidator
          (InlineDatum ())
          (amountOfAda 4_000_000 <> dummyTreasuryValue)

      -- Need to pay something to the payment address provided
      payToGeneralAddress =
        mconcat
          [ payToKey
              dummyGeneralPaymentAddress
              (adaValue 2)
          , userSpend spend3
          ]

      combinedTxs =
        mconcat
          [ baseTx
          , payToTreasuryValidator
          , payToGeneralAddress
          ]

  submitTx user combinedTxs
