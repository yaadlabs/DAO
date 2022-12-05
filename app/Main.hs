{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import           Cardano.Api hiding (TxId)
import           Options.Generic
import           Canonical.ConfigurationNft
import           Canonical.Vote
import           Prelude
import           Plutus.V1.Ledger.Bytes
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           Data.String

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
  { configurationNftOutput           :: FilePath
  , configurationNftPolicyIdOutput   :: FilePath
  , configurationNftTokenName        :: TokenName
  , configurationNftInitialUtxo      :: TxOutRef
  , configurationValidatorOutput     :: FilePath
  , configurationValidatorHashOutput :: FilePath
  , voteMinterOutput                 :: FilePath
  , voteMinterPolicyIdOutput         :: FilePath
  , voteValidatorOutput              :: FilePath
  , voteValidatorHashOutput          :: FilePath
  } deriving (Show, Generic)

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
run Options{..} = do
  let nftConfig = NftConfig
        { ncInitialUtxo = configurationNftInitialUtxo
        , ncTokenName   = configurationNftTokenName
        }

  writeSource configurationNftOutput $ nftMinter nftConfig

  let theConfigurationNftPolicyId = nftMinterPolicyId nftConfig

  writeFile configurationNftPolicyIdOutput $ show theConfigurationNftPolicyId

  ----
  let configurationValidatorConfig = ConfigurationValidatorConfig
        { cvcConfigNftCurrencySymbol = theConfigurationNftPolicyId
        , cvcConfigNftTokenName      = configurationNftTokenName
        }

  writeSource configurationValidatorOutput (configurationScript configurationValidatorConfig)

  writeFile configurationValidatorHashOutput $ show (configurationValidatorHash configurationValidatorConfig)

  ---
  let voteMinterConfig = VoteMinterConfig
        { vmcConfigNftCurrencySymbol = theConfigurationNftPolicyId
        , vmcConfigNftTokenName      = configurationNftTokenName
        }

  writeSource voteMinterOutput (voteMinter voteMinterConfig)

  let theVoteMinterCurrencySymbol = voteMinterPolicyId voteMinterConfig

  writeFile voteMinterPolicyIdOutput $ show theVoteMinterCurrencySymbol

  let voteValidatorConfig = VoteValidatorConfig
        { vvcConfigNftCurrencySymbol = theConfigurationNftPolicyId
        , vvcConfigNftTokenName      = configurationNftTokenName
        }

  writeSource voteValidatorOutput (voteScript voteValidatorConfig)

  writeFile voteValidatorHashOutput $ show (voteValidatorHash voteValidatorConfig)
