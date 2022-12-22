module Canonical.ConfigurationNft where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Crypto
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           PlutusTx
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           PlutusTx.Prelude
import qualified Plutonomy
import           Canonical.Types
import           Canonical.Shared



-------------------------------------------------------------------------------
-- Validator
-- The validator makes sure the config cannot be changed unless an upgrade
-- request is present
-- The idea is that
-- I need to get a reference to a tally nft
-- That includes datum that has a reference utxo
-- That Utxo includes an upgrade proposal
-- The upgrade proposal has a datum that includes a minter
-- That if everything is good unlocks the treasury
-- and makes sure a token is minted to validate the upgrade
-------------------------------------------------------------------------------

data ConfigurationAddress = ConfigurationAddress
  { cAddressCredential        :: Credential
  , cAddressStakingCredential :: BuiltinData
  }

data ConfigurationTxOut = ConfigurationTxOut
  { cTxOutAddress             :: ConfigurationAddress
  , cTxOutValue               :: Value
  , cTxOutDatum               :: OutputDatum
  , cTxOutReferenceScript     :: BuiltinData
  }

data ConfigurationTxInInfo = ConfigurationTxInInfo
  { cTxInInfoOutRef   :: TxOutRef
  , cTxInInfoResolved :: ConfigurationTxOut
  }

data ConfigurationScriptPurpose = ConfigurationSpend TxOutRef

data ConfigurationScriptContext = ConfigurationScriptContext
  { cScriptContextTxInfo  :: ConfigurationTxInfo
  , cScriptContextPurpose :: ConfigurationScriptPurpose
  }

data ConfigurationTxInfo = ConfigurationTxInfo
  { cTxInfoInputs             :: [ConfigurationTxInInfo]
  , cTxInfoReferenceInputs    :: [ConfigurationTxInInfo]
  , cTxInfoOutputs            :: [ConfigurationTxOut]
  , cTxInfoFee                :: BuiltinData
  , cTxInfoMint               :: Value
  , cTxInfoDCert              :: BuiltinData
  , cTxInfoWdrl               :: BuiltinData
  , cTxInfoValidRange         :: POSIXTimeRange
  , cTxInfoSignatories        :: [PubKeyHash]
  , cTxInfoRedeemers          :: BuiltinData
  , cTxInfoData               :: Map DatumHash Datum
  , cTxInfoId                 :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

data ConfigurationValidatorConfig = ConfigurationValidatorConfig
  { cvcConfigNftCurrencySymbol :: CurrencySymbol
  , cvcConfigNftTokenName      :: TokenName
  }

unstableMakeIsData ''ConfigurationAddress
unstableMakeIsData ''ConfigurationTxOut
unstableMakeIsData ''ConfigurationTxInInfo
makeIsDataIndexed  ''ConfigurationScriptPurpose [('ConfigurationSpend,1)]
unstableMakeIsData ''ConfigurationScriptContext
unstableMakeIsData ''ConfigurationTxInfo
makeLift ''ConfigurationValidatorConfig

ownValue :: [ConfigurationTxInInfo] -> TxOutRef -> Value
ownValue ins txOutRef = go ins where
  go = \case
    [] -> traceError "The impossible happened"
    ConfigurationTxInInfo {cTxInInfoOutRef, cTxInInfoResolved = ConfigurationTxOut{cTxOutValue}} :xs ->
      if cTxInInfoOutRef == txOutRef then
        cTxOutValue
      else
        go xs

validateConfiguration
  :: ConfigurationValidatorConfig
  -> DynamicConfig
  -> BuiltinData
  -> ConfigurationScriptContext
  -> Bool
validateConfiguration
  ConfigurationValidatorConfig {..}
  DynamicConfig {..}
  _
  ConfigurationScriptContext
    { cScriptContextTxInfo = ConfigurationTxInfo {..}
    , cScriptContextPurpose = ConfigurationSpend thisOutRef
    } =
  let
    thisScriptValue :: Value
    !thisScriptValue = ownValue cTxInfoInputs thisOutRef

    hasConfigurationNft :: Bool
    !hasConfigurationNft = case M.lookup cvcConfigNftCurrencySymbol (getValue thisScriptValue) of
      Nothing -> False
      Just m  -> case M.lookup cvcConfigNftTokenName m of
        Nothing -> False
        Just c -> c == 1

    hasTallyNft :: Value -> Bool
    hasTallyNft (Value v) = case M.lookup dcTallyNft v of
      Nothing -> False
      Just m  -> case M.lookup dcTallyTokenName m of
        Nothing -> False
        Just c -> c == 1

    TallyState {..} = case filter (hasTallyNft . cTxOutValue . cTxInInfoResolved) cTxInfoReferenceInputs of
      [] -> traceError "Missing tally NFT"
      [ConfigurationTxInInfo {cTxInInfoResolved = ConfigurationTxOut {..}}] -> unsafeFromBuiltinData $ case cTxOutDatum of
        OutputDatum (Datum dbs) -> dbs
        OutputDatumHash dh -> case M.lookup dh cTxInfoData of
          Just (Datum dbs) -> dbs
          _ -> traceError "Missing datum"
        NoOutputDatum -> traceError "Script input missing datum hash"
      _ -> traceError "Too many NFT values"

    totalVotes :: Integer
    !totalVotes = tsFor + tsAgainst

    relativeMajority :: Integer
    !relativeMajority = (totalVotes * 1000) `divide` dcTotalVotes

    majorityPercent :: Integer
    !majorityPercent = (tsFor * 1000) `divide` totalVotes

    hasEnoughVotes :: Bool
    !hasEnoughVotes
      =  traceIfFalse "relative majority is too low" (relativeMajority >= dcUpgradRelativeMajorityPercent)
      && traceIfFalse "majority is too small" (majorityPercent >= dcUpgradeMajorityPercent)

    -- Find a the reference input with using the tsProposal TxOutRef
    Proposal {pType = Upgrade {ptUpgradeMinter}, ..} = case filter ((==tsProposal) . cTxInInfoOutRef) cTxInfoReferenceInputs of
      [] -> traceError "Missing proposal NFT"
      [ConfigurationTxInInfo {cTxInInfoResolved = ConfigurationTxOut {..}}] -> convertDatum cTxInfoData cTxOutDatum
      _ -> traceError "Too many NFT values"

    -- Make sure the upgrade token was minted
    hasUpgradeMinterToken :: Bool
    !hasUpgradeMinterToken = case M.lookup ptUpgradeMinter (getValue cTxInfoMint) of
      Nothing -> False
      Just m  -> case M.toList m of
        [(_, c)] -> c == 1
        _ -> False

    isAfterTallyEndTime :: Bool
    isAfterTallyEndTime = (pEndTime + POSIXTime dcProposalTallyEndOffset) `before` cTxInfoValidRange

  in traceIfFalse "Missing configuration nft" hasConfigurationNft
  && traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
  && traceIfFalse "Not minting upgrade token" hasUpgradeMinterToken
  && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime

wrapValidateConfiguration
    :: ConfigurationValidatorConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateConfiguration cfg x y z = check (
  validateConfiguration
    cfg
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

configurationValidator :: ConfigurationValidatorConfig -> Validator
configurationValidator cfg = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateConfiguration ||])
    `applyCode`
    liftCode cfg

configurationValidatorHash :: ConfigurationValidatorConfig -> ValidatorHash
configurationValidatorHash = validatorHash . configurationValidator

configurationScript :: ConfigurationValidatorConfig ->  PlutusScript PlutusScriptV2
configurationScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . configurationValidator
