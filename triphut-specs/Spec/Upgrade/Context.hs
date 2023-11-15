module Spec.Upgrade.Context (validUpgradeTest) where

import Plutus.Model (
  Run,
  adaValue,
  currentTime,
  mintValue,
  newUser,
  refInputInline,
  spend,
  spendScript,
  submitTx,
  userSpend,
  validateIn,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToKey,
  payToScript,
 )
import Plutus.V1.Ledger.Interval (from)
import Plutus.V1.Ledger.Value (Value, adaToken, singleton)
import Spec.AlwaysSucceed.Script (
  alwaysSucceedCurrencySymbol,
  alwaysSucceedTypedMintingPolicy,
 )
import Spec.ConfigurationNft.Script (upgradeConfigNftTypedValidator)
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.Transactions (runInitUpgradeTallyWithEndTimeInPast)
import Spec.Tally.Utils (findTally)
import Spec.Values (dummyConfigNftValue)
import Triphut.Types (DynamicConfigDatum (DynamicConfigDatum))
import Prelude (mconcat, pure, ($), (<>))

validUpgradeTest :: Run ()
validUpgradeTest = mkUpgradeTest

mkUpgradeTest :: Run ()
mkUpgradeTest = do
  runInitConfig
  runInitUpgradeTallyWithEndTimeInPast

  (configOutRef, _, configDatum) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findTally

  user <- newUser $ amountOfAda 8_000_000
  spend1 <- spend user $ amountOfAda 4_000_000
  spend2 <- spend user $ amountOfAda 2_000_002

  theTimeNow <- currentTime

  let
    upgradeToken :: Value
    upgradeToken = singleton alwaysSucceedCurrencySymbol adaToken 1

    baseTx =
      mconcat
        [ spendScript upgradeConfigNftTypedValidator configOutRef () configDatum
        , mintValue alwaysSucceedTypedMintingPolicy () upgradeToken
        , refInputInline tallyOutRef
        , userSpend spend1
        , userSpend spend2
        ]

    payToConfigValidator =
      payToScript
        upgradeConfigNftTypedValidator
        (InlineDatum configDatum)
        (adaValue 2 <> dummyConfigNftValue)

    -- Pay it to the user, just for balancing the tx for now
    -- Not sure what is meant to happen with this token after minting it here
    payUpgradeTokenToUser = payToKey user upgradeToken

    combinedTxs =
      mconcat
        [ baseTx
        , payToConfigValidator
        , payUpgradeTokenToUser
        ]

  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx
