{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Canonical.AlwaysSucceed (succeed, succeed1, succeedHash, succeedHash1)
import Canonical.ConfigurationNft (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
  NftConfig (NftConfig, ncInitialUtxo, ncTokenName),
  configurationScript,
  configurationValidatorHash,
  nftMinter,
  nftMinterPolicyId,
 )
import Canonical.Tally (
  IndexNftConfig (IndexNftConfig, incIndexValidator, incInitialUtxo, incTokenName),
  IndexValidatorConfig (
    IndexValidatorConfig,
    ivcConfigNftCurrencySymbol,
    ivcConfigNftTokenName,
    ivcNonce
  ),
  TallyNftConfig (
    TallyNftConfig,
    tncConfigNftCurrencySymbol,
    tncConfigNftTokenName,
    tncIndexNftPolicyId,
    tncIndexNftTokenName
  ),
  TallyValidatorConfig (
    TallyValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
  indexScript,
  indexValidatorHash,
  tallyIndexNftMinter,
  tallyIndexNftMinterPolicyId,
  tallyNftMinter,
  tallyNftMinterPolicyId,
  tallyScript,
  tallyValidatorHash,
 )
import Canonical.Treasury (
  TreasuryValidatorConfig (
    TreasuryValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
  treasuryScript,
  treasuryValidatorHash,
 )
import Canonical.Vote (
  VoteMinterConfig (
    VoteMinterConfig,
    vmcConfigNftCurrencySymbol,
    vmcConfigNftTokenName
  ),
  VoteValidatorConfig (
    VoteValidatorConfig,
    vvcConfigNftCurrencySymbol,
    vvcConfigNftTokenName
  ),
  voteMinter,
  voteMinterPolicyId,
  voteScript,
  voteValidatorHash,
 )
import Cardano.Api hiding (TxId)
import Data.String (fromString)
import Options.Generic (
  Generic,
  ParseField,
  ParseFields,
  ParseRecord,
  getOnly,
  getRecord,
  lispCaseModifiers,
  parseField,
  parseFields,
  parseRecord,
  parseRecordWithModifiers,
  readField,
 )
import Plutus.V1.Ledger.Bytes (getLedgerBytes)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Tx (TxId (TxId), TxOutRef (TxOutRef))
import Plutus.V1.Ledger.Value (TokenName)
import Prelude (
  Either (Left, Right),
  FilePath,
  IO,
  Integer,
  Maybe (Nothing),
  Read,
  Show,
  fmap,
  print,
  putStrLn,
  read,
  readsPrec,
  show,
  span,
  tail,
  writeFile,
  ($),
  (++),
  (/=),
  (<$>),
  (=<<),
  (>>=),
 )

instance Read TokenName where
  readsPrec _ x = [(fromString x, "")]
instance ParseRecord TokenName where
  parseRecord = fmap getOnly parseRecord
instance ParseField TokenName
instance ParseFields TokenName

instance Read ValidatorHash where
  readsPrec _ x = [(fromString x, "")]

instance ParseRecord ValidatorHash where
  parseRecord = fmap getOnly parseRecord
instance ParseField ValidatorHash
instance ParseFields ValidatorHash

data Options = Options
  { alwaysSucceedOutput :: FilePath
  , alwaysSucceedHashOutput :: FilePath
  , alwaysSucceed1Output :: FilePath
  , alwaysSucceed1HashOutput :: FilePath
  , configurationNftOutput :: FilePath
  , configurationNftPolicyIdOutput :: FilePath
  , configurationNftTokenName :: TokenName
  , configurationNftInitialUtxo :: TxOutRef
  , configurationValidatorOutput :: FilePath
  , configurationValidatorHashOutput :: FilePath
  , voteMinterOutput :: FilePath
  , voteMinterPolicyIdOutput :: FilePath
  , voteValidatorOutput :: FilePath
  , voteValidatorHashOutput :: FilePath
  , treasuryValidatorOutput :: FilePath
  , treasuryValidatorHashOutput :: FilePath
  , tallyIndexNftOutput :: FilePath
  , tallyIndexNftPolicyIdOutput :: FilePath
  , tallyIndexNftTokenName :: TokenName
  , tallyIndexNftInitialUtxo :: TxOutRef
  , tallyNftOutput :: FilePath
  , tallyNftPolicyIdOutput :: FilePath
  , indexValidatorOutput :: FilePath
  , indexValidatorHashOutput :: FilePath
  , tallyValidatorOutput :: FilePath
  , tallyValidatorHashOutput :: FilePath
  , indexValidatorNonce :: Integer
  }
  deriving (Show, Generic)

instance ParseField PubKeyHash where
  parseField x y z w = fromString <$> parseField x y z w
  readField = fromString <$> readField

instance ParseFields PubKeyHash where
  parseFields x y z w = fromString <$> parseFields x y z w

instance ParseRecord PubKeyHash where
  parseRecord = fromString <$> parseRecord

instance Read TxOutRef where
  readsPrec _ s =
    let
      (x, y) = span (/= '#') s
     in
      [(TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y, "")]
instance ParseRecord TxOutRef where
  parseRecord = fmap getOnly parseRecord
instance ParseField TxOutRef
instance ParseFields TxOutRef

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = run =<< getRecord "Triphut compiler"

writeSource :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeSource outputPath source =
  writeFileTextEnvelope outputPath Nothing source >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ outputPath

run :: Options -> IO ()
run Options {..} = do
  writeSource alwaysSucceedOutput succeed

  writeFile alwaysSucceedHashOutput $ show succeedHash

  writeSource alwaysSucceed1Output succeed1

  writeFile alwaysSucceed1HashOutput $ show succeedHash1

  let nftConfig =
        NftConfig
          { ncInitialUtxo = configurationNftInitialUtxo
          , ncTokenName = configurationNftTokenName
          }

  writeSource configurationNftOutput $ nftMinter nftConfig

  let theConfigurationNftPolicyId = nftMinterPolicyId nftConfig

  writeFile configurationNftPolicyIdOutput $ show theConfigurationNftPolicyId

  ----
  let configurationValidatorConfig =
        ConfigurationValidatorConfig
          { cvcConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , cvcConfigNftTokenName = configurationNftTokenName
          }

  writeSource configurationValidatorOutput (configurationScript configurationValidatorConfig)

  writeFile configurationValidatorHashOutput $ show (configurationValidatorHash configurationValidatorConfig)

  ---
  let voteMinterConfig =
        VoteMinterConfig
          { vmcConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , vmcConfigNftTokenName = configurationNftTokenName
          }

  writeSource voteMinterOutput (voteMinter voteMinterConfig)

  let theVoteMinterCurrencySymbol = voteMinterPolicyId voteMinterConfig

  writeFile voteMinterPolicyIdOutput $ show theVoteMinterCurrencySymbol

  let voteValidatorConfig =
        VoteValidatorConfig
          { vvcConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , vvcConfigNftTokenName = configurationNftTokenName
          }

  writeSource voteValidatorOutput (voteScript voteValidatorConfig)

  writeFile voteValidatorHashOutput $ show (voteValidatorHash voteValidatorConfig)

  let treasuryValidatorConfig =
        TreasuryValidatorConfig
          { tvcConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , tvcConfigNftTokenName = configurationNftTokenName
          }

  writeSource treasuryValidatorOutput (treasuryScript treasuryValidatorConfig)

  writeFile treasuryValidatorHashOutput $ show (treasuryValidatorHash treasuryValidatorConfig)

  let indexValidatorConfig =
        IndexValidatorConfig
          { ivcConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , ivcConfigNftTokenName = configurationNftTokenName
          , ivcNonce = indexValidatorNonce
          }

  writeSource indexValidatorOutput (indexScript indexValidatorConfig)

  let theIndexValidatorHash = indexValidatorHash indexValidatorConfig

  writeFile indexValidatorHashOutput $ show theIndexValidatorHash

  let indexNftConfig =
        IndexNftConfig
          { incInitialUtxo = tallyIndexNftInitialUtxo
          , incTokenName = tallyIndexNftTokenName
          , incIndexValidator = theIndexValidatorHash
          }

  writeSource tallyIndexNftOutput $ tallyIndexNftMinter indexNftConfig

  let theTallyIndexNftPolicyId = tallyIndexNftMinterPolicyId indexNftConfig

  writeFile tallyIndexNftPolicyIdOutput $ show theTallyIndexNftPolicyId

  let tallyNftConfig =
        TallyNftConfig
          { tncIndexNftPolicyId = theTallyIndexNftPolicyId
          , tncIndexNftTokenName = tallyIndexNftTokenName
          , tncConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , tncConfigNftTokenName = configurationNftTokenName
          }

  writeSource tallyNftOutput $ tallyNftMinter tallyNftConfig

  let theTallyNftPolicyId = tallyNftMinterPolicyId tallyNftConfig

  writeFile tallyNftPolicyIdOutput $ show theTallyNftPolicyId

  let tallyValidatorConfig =
        TallyValidatorConfig
          { tvcConfigNftCurrencySymbol = theConfigurationNftPolicyId
          , tvcConfigNftTokenName = configurationNftTokenName
          }

  writeSource tallyValidatorOutput (tallyScript tallyValidatorConfig)

  let theTallyValidatorHash = tallyValidatorHash tallyValidatorConfig

  writeFile tallyValidatorHashOutput $ show theTallyValidatorHash
