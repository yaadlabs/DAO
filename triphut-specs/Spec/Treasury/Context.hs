module Spec.Treasury.Context (validTreasuryTest) where

import Control.Monad (void)
import Plutus.Model (
  Run,
  adaValue,
  currentTime,
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
  payToScript,
  refInputInline,
 )
import Plutus.V1.Ledger.Interval (from)
import PlutusTx.Prelude (($))
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.Transactions (runInitTripTallyWithEndTimeInFuture)
import Spec.Tally.Utils (findTally)
import Spec.Treasury.Script (treasuryTypedValidator)
import Spec.Treasury.Transactions (runInitTreasury)
import Spec.Treasury.Utils (findTreasury)
import Spec.Values (dummyTreasuryValue)
import Triphut.Vote (VoteActionRedeemer (Count))
import Prelude (mconcat, pure, show, (*), (+), (<>))

validTreasuryTest :: Run ()
validTreasuryTest = mkTreasuryTest

mkTreasuryTest :: Run ()
mkTreasuryTest = do
  runInitConfig
  runInitTripTallyWithEndTimeInFuture
  runInitTreasury

  (configOutRef, _, _) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findTally
  (treasuryOutRef, _, _) <- findTreasury

  user <- newUser $ amountOfAda 8_000_000
  spend1 <- spend user $ amountOfAda 4_000_000
  spend2 <- spend user $ amountOfAda 2_000_002

  -- theTimeNow <- currentTime

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
          (adaValue 2 <> dummyTreasuryValue)

      combinedTxs = baseTx <> payToTreasuryValidator

  -- finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user combinedTxs
