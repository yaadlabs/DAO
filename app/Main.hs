{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

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
import Dao.AlwaysSucceed (succeed, succeed1, succeedHash, succeedHash1)
import Dao.ConfigurationNft (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
  NftConfig (NftConfig, ncInitialUtxo, ncTokenName),
 )
import Dao.ConfigurationNft.Script (
  configurationNftCurrencySymbol,
  configurationNftMintingPolicy,
  configurationScript,
  configurationValidatorHash,
 )
import Dao.Index (
  IndexNftConfig (IndexNftConfig, incIndexValidator, incInitialUtxo, incTokenName),
 )
import Dao.Index.Script (
  indexScript,
  indexValidatorHash,
  tallyIndexNftMinter,
  tallyIndexNftMinterPolicyId,
 )
import Dao.Tally (
  TallyNftConfig (
    TallyNftConfig,
    tncConfigNftCurrencySymbol,
    tncConfigNftTokenName,
    tncIndexNftPolicyId,
    tncIndexNftTokenName
  ),
 )
import Dao.Tally.Script (
  tallyNftMinter,
  tallyNftMinterPolicyId,
  tallyScript,
  tallyValidatorHash,
 )
import Dao.Treasury.Script (treasuryScript, treasuryValidatorHash)
import Dao.Vote.Script (
  voteMinter,
  voteMinterPolicyId,
  voteScript,
  voteValidatorHash,
 )
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
  deriving stock (Show, Generic)

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
main = run =<< getRecord "Dao compiler"

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

  writeSource configurationNftOutput $ configurationNftMintingPolicy nftConfig

  let theConfigurationNftPolicyId = configurationNftCurrencySymbol nftConfig

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

  writeSource voteMinterOutput (voteMinter configurationValidatorConfig)

  let theVoteMinterCurrencySymbol = voteMinterPolicyId configurationValidatorConfig

  writeFile voteMinterPolicyIdOutput $ show theVoteMinterCurrencySymbol

  writeSource voteValidatorOutput (voteScript configurationValidatorConfig)

  writeFile voteValidatorHashOutput $ show (voteValidatorHash configurationValidatorConfig)

  writeSource treasuryValidatorOutput (treasuryScript configurationValidatorConfig)

  writeFile treasuryValidatorHashOutput $ show (treasuryValidatorHash configurationValidatorConfig)

  writeSource indexValidatorOutput indexScript

  let theIndexValidatorHash = indexValidatorHash

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

  writeSource tallyValidatorOutput (tallyScript configurationValidatorConfig)

  let theTallyValidatorHash = tallyValidatorHash configurationValidatorConfig

  writeFile tallyValidatorHashOutput $ show theTallyValidatorHash
