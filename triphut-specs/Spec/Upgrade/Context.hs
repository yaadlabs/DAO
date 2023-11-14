module Spec.Upgrade.Context (validUpgradeTest) where

import Plutus.Model (
  Run,
  adaValue,
  newUser,
  refInputInline,
  spend,
  spendScript,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToScript,
 )
import Spec.ConfigurationNft.Script (upgradeConfigNftTypedValidator)
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.SpecUtils (minAda)
import Spec.Tally.Transactions (runInitTally)
import Spec.Tally.Utils (findTally)
import Spec.Values (dummyConfigNftValue)
import Triphut.Types (DynamicConfigDatum (DynamicConfigDatum))
import Prelude (mconcat, pure, ($), (<>))

validUpgradeTest :: Run ()
validUpgradeTest = mkUpgradeTest

mkUpgradeTest :: Run ()
mkUpgradeTest = do
  runInitConfig
  runInitTally

  (configOutRef, _, configDatum) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findTally

  user <- newUser minAda
  spend' <- spend user (adaValue 2)
  spend2 <- spend user (adaValue 3)

  let baseTx =
        mconcat
          [ spendScript upgradeConfigNftTypedValidator configOutRef () configDatum
          , refInputInline tallyOutRef
          , userSpend spend'
          , userSpend spend2
          ]

      payToConfigValidator =
        payToScript
          upgradeConfigNftTypedValidator
          (InlineDatum configDatum)
          (adaValue 2 <> dummyConfigNftValue)

  submitTx user $ baseTx <> payToConfigValidator
