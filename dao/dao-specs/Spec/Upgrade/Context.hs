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
  spend,
  spendScript,
  submitTx,
  userSpend,
  validateIn,
 )
import Plutus.Model.V2 (DatumMode (InlineDatum), payToScript)
import PlutusLedgerApi.V1.Interval (from)
import PlutusLedgerApi.V1.Value (singleton)
import Spec.AlwaysSucceed.Script (alwaysSucceedCurrencySymbol, alwaysSucceedTypedMintingPolicy)
import Spec.Configuration.Script (upgradeConfigNftTypedValidator)
import Spec.Configuration.Transactions (
  runHighRelativeMajorityTotalVotesInitConfig,
  runInitConfig,
  runInitTreasuryTestConfig,
 )
import Spec.Configuration.Utils (findConfig)
import Spec.Index.Transactions (runInitIndex)
import Spec.SpecUtils (amountOfAda, findConfigUtxo, runInitPayToScript)
import Spec.Tally.SampleData (
  sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum,
  sampleUpgradeWithVotesEndTimeInPastTallyStateDatum,
 )
import Spec.Tally.Script (tallyNftTypedValidator)
import Spec.Values (dummyConfigNftValue, dummyTallyTokenName)
import Spec.Vote.Transactions (runInitVoteNft)
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
  = UpgradeTokenMinted -- Valid (Tally NFT burned)
  | NoUpgradeTokenMinted -- Invalid (Tally NFT not burned)
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
  -- Use special Treasury test config that has alwaysSucceed for tally burning (ID-501)
  when (enoughVotes == HasEnoughVotes) runInitTreasuryTestConfig
  when (enoughVotes == NotEnoughVotes) runHighRelativeMajorityTotalVotesInitConfig

  -- Initialize Index (required for proper test setup, matches successful Tally test pattern)
  runInitIndex

  -- Initialize vote NFT before Tally initialization (required for Tally NFT minting)
  runInitVoteNft

  -- Use simplified dummy Tally initialization with alwaysSucceed token
  -- Use samples with votes to avoid division by zero in vote percentage calculations
  let tallyValue = singleton alwaysSucceedCurrencySymbol dummyTallyTokenName 1
  when (enoughVotes == HasEnoughVotes) $
    runInitPayToScript tallyNftTypedValidator sampleUpgradeWithVotesEndTimeInPastTallyStateDatum tallyValue
  when (enoughVotes == NotEnoughVotes) $
    runInitPayToScript tallyNftTypedValidator sampleUpgradeNotEnoughVotesEndTimeInPastTallyStateDatum tallyValue

  (configOutRef, _, configDatum) <- findConfig
  (tallyOutRef, _, tallyDatum) <- findConfigUtxo tallyNftTypedValidator alwaysSucceedCurrencySymbol dummyTallyTokenName

  user <- newUser $ amountOfAda 3_000_000

  theTimeNow <- currentTime

  let
    baseTx = mempty -- No user spending needed - scripts provide all the ADA

    -- Spend tally input when required (ID-501: Tally must be spent, not referenced)
    withTallyInput
      | tallyReference == TallyIncluded = spendScript tallyNftTypedValidator tallyOutRef () tallyDatum
      | otherwise = mempty

    -- Burn Tally NFT when required (ID-503: replaces upgrade token, ID-501: prevents reuse)
    -- Using alwaysSucceed policy for simplified testing
    withTallyBurn
      | upgradeMinted == UpgradeTokenMinted =
          mintValue
            alwaysSucceedTypedMintingPolicy
            ()
            (singleton alwaysSucceedCurrencySymbol dummyTallyTokenName (-1))
      | otherwise = mempty

    -- Spend config input for valid test
    withConfigInput
      | configInput == ConfigIncluded = spendScript upgradeConfigNftTypedValidator configOutRef () configDatum
      | otherwise = mempty

    -- Config has 2M + Tally has 2M = 4M total ADA from scripts
    -- Return all 4M with the config NFT (tally token is burned per ID-501)
    payToConfigValidator =
      payToScript
        upgradeConfigNftTypedValidator
        (InlineDatum configDatum)
        (amountOfAda 4_000_000 <> dummyConfigNftValue)

    combinedTxs =
      mconcat
        [ baseTx
        , payToConfigValidator
        , withTallyInput
        , withTallyBurn
        , withConfigInput
        ]

  finalTx <- validateIn (from theTimeNow) combinedTxs

  submitTx user finalTx
