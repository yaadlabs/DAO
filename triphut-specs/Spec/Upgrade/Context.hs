module Spec.Upgrade.Context (
  validUpgradeTest,
  invalidUpgradeNoTallyReferenceTest,
  invalidUpgradeNoConfigInputTest,
  invalidUpgradeNoUpgradeTokenMintedTest,
  invalidUpgradeNotEnoughVotesTest,
) where

import Control.Monad (when)
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
import Spec.ConfigurationNft.Transactions (
  runHighRelativeMajorityTotalVotesInitConfig,
  runInitConfig,
 )
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.SpecUtils (amountOfAda)
import Spec.Tally.Transactions (
  runInitUpgradeTallyWithEndTimeInPast,
  runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes,
 )
import Spec.Tally.Utils (findTally)
import Spec.Values (dummyConfigNftValue)
import Prelude (Eq, mconcat, mempty, otherwise, ($), (<>), (==))

-- | Positive test
validUpgradeTest :: Run ()
validUpgradeTest =
  mkUpgradeTest
    TallyIncluded
    ConfigIncluded
    UpgradeTokenMinted
    HasEnoughVotes

-- | Negative tests
invalidUpgradeNoTallyReferenceTest :: Run ()
invalidUpgradeNoTallyReferenceTest =
  mkUpgradeTest
    TallyNotIncluded
    ConfigIncluded
    UpgradeTokenMinted
    HasEnoughVotes

invalidUpgradeNoConfigInputTest :: Run ()
invalidUpgradeNoConfigInputTest =
  mkUpgradeTest
    TallyIncluded
    ConfigNotIncluded
    UpgradeTokenMinted
    HasEnoughVotes

invalidUpgradeNoUpgradeTokenMintedTest :: Run ()
invalidUpgradeNoUpgradeTokenMintedTest =
  mkUpgradeTest
    TallyIncluded
    ConfigIncluded
    NoUpgradeTokenMinted
    HasEnoughVotes

invalidUpgradeNotEnoughVotesTest :: Run ()
invalidUpgradeNotEnoughVotesTest =
  mkUpgradeTest
    TallyIncluded
    ConfigIncluded
    UpgradeTokenMinted
    NotEnoughVotes

data TallyReference
  = TallyIncluded -- Valid
  | TallyNotIncluded -- Invalid
  deriving stock (Eq)

data ConfigInput
  = ConfigIncluded -- Valid
  | ConfigNotIncluded -- Invalid
  deriving stock (Eq)

data UpgradeTokenMinted
  = UpgradeTokenMinted -- Valid
  | NoUpgradeTokenMinted -- Invalid
  deriving stock (Eq)

data EnoughVotes
  = HasEnoughVotes -- Valid
  | NotEnoughVotes -- Invalid
  deriving stock (Eq)

mkUpgradeTest ::
  TallyReference ->
  ConfigInput ->
  UpgradeTokenMinted ->
  EnoughVotes ->
  Run ()
mkUpgradeTest tallyReference configInput upgradeMinted enoughVotes = do
  -- Choose which config to load based on whether we want to trigger
  -- the negative test for not enough votes or not
  when (enoughVotes == HasEnoughVotes) runInitConfig
  when (enoughVotes == NotEnoughVotes) runHighRelativeMajorityTotalVotesInitConfig

  -- Choose which tally to load based on whether we want to trigger
  -- the negative test for not enough votes or not
  when (enoughVotes == HasEnoughVotes) runInitUpgradeTallyWithEndTimeInPast
  when (enoughVotes == NotEnoughVotes) runInitUpgradeTallyWithEndTimeInPastNotEnoughVotes

  (configOutRef, _, configDatum) <- findConfig
  (tallyOutRef, _, _) <- findTally

  user <- newUser $ amountOfAda 8_000_000
  spend1 <- spend user $ amountOfAda 4_000_000
  spend2 <- spend user $ amountOfAda 2_000_002

  theTimeNow <- currentTime

  let
    upgradeToken :: Value
    upgradeToken = singleton alwaysSucceedCurrencySymbol adaToken 1

    baseTx =
      mconcat
        [ userSpend spend1
        , userSpend spend2
        ]

    -- Mint upgrade token for valid test
    withUpgradeTokenMinted
      | upgradeMinted == UpgradeTokenMinted = mintValue alwaysSucceedTypedMintingPolicy () upgradeToken
      | otherwise = mempty

    -- Spend config input for valid test
    withConfigInput
      | configInput == ConfigIncluded = spendScript upgradeConfigNftTypedValidator configOutRef () configDatum
      | otherwise = mempty

    -- Include tally in the reference inputs for valid test
    withTallyReference
      | tallyReference == TallyIncluded = refInputInline tallyOutRef
      | otherwise = mempty

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
        , withTallyReference
        , withConfigInput
        , withUpgradeTokenMinted
        ]

  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx
