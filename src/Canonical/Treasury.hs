module Canonical.Treasury where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
-- import           Plutus.V1.Ledger.Address
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Tx hiding (Mint)
import           PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as M
import           PlutusTx
import           PlutusTx.Prelude
import           Canonical.Shared
import qualified Canonical.Types as T
import qualified Plutonomy

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------
data TreasuryAddress = TreasuryAddress
  { tAddressCredential        :: Credential
  , tAddressStakingCredential :: BuiltinData
  }

data TreasuryTxOut = TreasuryTxOut
  { tTxOutAddress             :: TreasuryAddress
  , tTxOutValue               :: Value
  , tTxOutDatum               :: OutputDatum
  , tTxOutReferenceScript     :: BuiltinData
  }

data TreasuryTxInInfo = TreasuryTxInInfo
  { tTxInInfoOutRef   :: TxOutRef
  , tTxInInfoResolved :: TreasuryTxOut
  }

data TreasuryScriptPurpose = TreasurySpend TxOutRef

data TreasuryScriptContext = TreasuryScriptContext
  { tScriptContextTxInfo  :: TreasuryTxInfo
  , tScriptContextPurpose :: TreasuryScriptPurpose
  }

data TreasuryTxInfo = TreasuryTxInfo
  { tTxInfoInputs             :: [TreasuryTxInInfo]
  , tTxInfoReferenceInputs    :: [TreasuryTxInInfo]
  , tTxInfoOutputs            :: [TreasuryTxOut]
  , tTxInfoFee                :: BuiltinData
  , tTxInfoMint               :: Value
  , tTxInfoDCert              :: BuiltinData
  , tTxInfoWdrl               :: BuiltinData
  , tTxInfoValidRange         :: POSIXTimeRange
  , tTxInfoSignatories        :: [PubKeyHash]
  , tTxInfoRedeemers          :: BuiltinData
  , tTxInfoData               :: Map DatumHash Datum
  , tTxInfoId                 :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

type Treasury = BuiltinData

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName      :: TokenName
  }

unstableMakeIsData ''TreasuryAddress
unstableMakeIsData ''TreasuryTxOut
unstableMakeIsData ''TreasuryTxInInfo
makeIsDataIndexed  ''TreasuryScriptPurpose [('TreasurySpend,1)]
unstableMakeIsData ''TreasuryScriptContext
unstableMakeIsData ''TreasuryTxInfo
makeLift ''TreasuryValidatorConfig

-- Needs to work in bulk
-- TODO
-- This needs to upgradeable A.
-- So if there is an upgrade proposal
-- It just unlocks assuming there is a
-- Good tally
-- So it is basically the configuration validator logic
-- There are other proposals it does other things
validateTreasury
  :: TreasuryValidatorConfig
  -> Treasury
  -> BuiltinData
  -> TreasuryScriptContext
  -> Bool
validateTreasury
  TreasuryValidatorConfig {..}
  _treasury
  _action
  TreasuryScriptContext
    { tScriptContextTxInfo = TreasuryTxInfo {..}
    } =
  let
    hasConfigurationNft :: Value -> Bool
    hasConfigurationNft (Value v) = case M.lookup tvcConfigNftCurrencySymbol v of
      Nothing -> False
      Just m  -> case M.lookup tvcConfigNftTokenName m of
        Nothing -> False
        Just c -> c == 1

    -- filter the reference inputs for the configuration nft
    T.DynamicConfig {..} = case filter (hasConfigurationNft . tTxOutValue . tTxInInfoResolved) tTxInfoReferenceInputs of
      [TreasuryTxInInfo {tTxInInfoResolved = TreasuryTxOut {..}}] -> unsafeFromBuiltinData $ case tTxOutDatum of
        OutputDatum (Datum dbs) -> dbs
        OutputDatumHash dh -> case M.lookup dh tTxInfoData of
          Just (Datum dbs) -> dbs
          _ -> traceError "Missing datum"
        NoOutputDatum -> traceError "Script input missing datum hash"
      _ -> traceError "Too many NFT values"

    hasTallyNft :: Value -> Bool
    hasTallyNft (Value v) = case M.lookup dcTallyNft v of
      Nothing -> False
      Just {} -> True

    T.TallyState {..} = case filter (hasTallyNft . tTxOutValue . tTxInInfoResolved) tTxInfoReferenceInputs of
      [] -> traceError "Missing tally NFT"
      [TreasuryTxInInfo {tTxInInfoResolved = TreasuryTxOut {..}}] -> unsafeFromBuiltinData $ case tTxOutDatum of
        OutputDatum (Datum dbs) -> dbs
        OutputDatumHash dh -> case M.lookup dh tTxInfoData of
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

  in case tsProposal of
      -- TravelDisbursement -> error ()
      -- GeneralDisbursement -> error ()
      T.Upgrade upgradeMinter ->
        let
          hasEnoughVotes :: Bool
          !hasEnoughVotes
            =  traceIfFalse "relative majority is too low" (relativeMajority >= dcUpgradRelativeMajorityPercent)
            && traceIfFalse "majority is too small" (majorityPercent >= dcUpgradeMajorityPercent)

          -- Make sure the upgrade token was minted
          hasUpgradeMinterToken :: Bool
          !hasUpgradeMinterToken = case M.lookup upgradeMinter (getValue tTxInfoMint) of
            Nothing -> False
            Just m  -> case M.toList m of
              [(_, c)] -> c == 1
              _ -> False

          isAfterTallyEndTime :: Bool
          isAfterTallyEndTime = (tsProposalEndTime + POSIXTime dcProposalTallyEndOffset) `before` tTxInfoValidRange

        in traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
        && traceIfFalse "Not minting upgrade token" hasUpgradeMinterToken
        && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime

wrapValidateTreasury
    :: TreasuryValidatorConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateTreasury cfg x y z = check (
  validateTreasury
    cfg
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

treasuryValidator :: TreasuryValidatorConfig -> Validator
treasuryValidator cfg = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay      = False
      , Plutonomy.ooFloatOutLambda  = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateTreasury ||])
    `applyCode`
    liftCode cfg

treasuryValidatorHash :: TreasuryValidatorConfig -> ValidatorHash
treasuryValidatorHash = validatorHash . treasuryValidator

treasuryScript :: TreasuryValidatorConfig ->  PlutusScript PlutusScriptV2
treasuryScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . treasuryValidator
