module Canonical.Tally where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Credential
import           Plutus.V1.Ledger.Crypto
-- import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Scripts
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           PlutusTx
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           PlutusTx.Prelude
import qualified Plutonomy
import           Canonical.Shared
import           Canonical.Types
-- TODO use the Tally NFT index
-- When making the Tally utxo it needs to make an NFT, that is
-- in the proposal datum, and the proposal utxo is in the tally datum
-- the proposal needs to stay locked
-- I think I should probably combine the proposals and the tallys
-- so there is only one utxo
-- Do I even need an NFT in that case?
-- Yes I need an NFT for voting
-- So the idea is mint a regular NFT
-- Put in on the proposal/tally
-- Votes refer to it

-- So there is an index NFT that is used to make the tokenname
-- When validating you just need to make sure the policy correct

data IndexNftDatum = IndexNftDatum
  { indIndex :: Integer
  }

data IndexNftConfig = IndexNftConfig
  { incInitialUtxo    :: TxOutRef
  , incTokenName      :: TokenName
  , incIndexValidator :: ValidatorHash
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig

-- TODO this needs to send the value to the validator
mkIndexNftMinter :: IndexNftConfig -> BuiltinData -> ScriptContext -> Bool
mkIndexNftMinter IndexNftConfig {..} _ ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } =
  let
    hasWitness :: Value -> Bool
    hasWitness (Value v) = case M.lookup thisCurrencySymbol v of
      Just m -> case M.toList m of
        [(_, c)] -> if c == 1 then True else traceError "wrong token count"
        _ -> traceError "wrong number of tokens with policy id"
      _ -> False

    hasUTxO :: Bool
    !hasUTxO = any (\i -> txInInfoOutRef i == incInitialUtxo) txInfoInputs

    (!IndexNftDatum{..}, !outputAddress) = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
      [ TxOut { .. } ] -> (convertDatum txInfoData txOutDatum, txOutAddress)
      _ -> traceError "Impossible. No minted output."

    initialIndexIsZero :: Bool
    !initialIndexIsZero = indIndex == 0

    onlyOneTokenMinted :: Bool
    !onlyOneTokenMinted =
      hasSingleToken
        txInfoMint
        thisCurrencySymbol
        incTokenName

    outputIsValidator :: Bool
    outputIsValidator = addressCredential outputAddress == ScriptCredential incIndexValidator

  in traceIfFalse "Missing significant UTxO!" hasUTxO
  && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted
  && traceIfFalse "Initial Index is not zero" initialIndexIsZero
  && traceIfFalse "Output is not index validator" outputIsValidator

mkIndexNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: IndexNftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkIndexNftMinter config a (unsafeFromBuiltinData b))

policy :: IndexNftConfig -> MintingPolicy
policy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicy c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

plutusScript :: IndexNftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: IndexNftConfig -> Validator
validator = Validator . plutusScript

tallyIndexNftMinterPolicyId :: IndexNftConfig -> CurrencySymbol
tallyIndexNftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: IndexNftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

tallyIndexNftMinter :: IndexNftConfig -> PlutusScript PlutusScriptV2
tallyIndexNftMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . scriptAsCbor

-------------------------------------------------------------------------------
-- Nft Index Validator
-------------------------------------------------------------------------------

data IndexValidatorConfig = IndexValidatorConfig
  { ivcConfigNftCurrencySymbol :: CurrencySymbol
  , ivcConfigNftTokenName      :: TokenName
  }

makeLift ''IndexValidatorConfig

validateIndex
  :: IndexValidatorConfig
  -> IndexNftDatum
  -> BuiltinData
  -> ScriptContext
  -> Bool
validateIndex
  IndexValidatorConfig {}
  IndexNftDatum { indIndex = inputIndex}
  _
  ctx@ScriptContext
    { scriptContextTxInfo = info@TxInfo {..}
    , scriptContextPurpose = Spending thisOutRef
    } =
  let
    scriptValue :: Value
    !scriptValue = case findTxInByTxOutRef thisOutRef info of
      Nothing -> traceError "Impossible not input"
      Just TxInInfo { txInInfoResolved = TxOut {..}} -> txOutValue

    outputValue :: Value
    (!outputValue, !IndexNftDatum { indIndex = outputIndex }) = case getContinuingOutputs ctx of
      [TxOut{..}] -> (txOutValue, convertDatum txInfoData txOutDatum)
      _ -> traceError "wrong number of continuing outputs"

    outputValueGreaterThanInputValue :: Bool
    outputValueGreaterThanInputValue = outputValue `geq` scriptValue

    outputDatumIsIncremented :: Bool
    outputDatumIsIncremented = outputIndex == inputIndex + 1

  in traceIfFalse "output datum is not incremented" outputDatumIsIncremented
  && traceIfFalse "script value is not returned" outputValueGreaterThanInputValue

validateIndex _ _ _ _ = traceError "Wrong script purpose"

wrapValidateIndex
    :: IndexValidatorConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateIndex cfg x y z = check (
  validateIndex
    cfg
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

indexValidator :: IndexValidatorConfig -> Validator
indexValidator cfg = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateIndex ||])
    `applyCode`
    liftCode cfg

indexValidatorHash :: IndexValidatorConfig -> ValidatorHash
indexValidatorHash = validatorHash . indexValidator

indexScript :: IndexValidatorConfig ->  PlutusScript PlutusScriptV2
indexScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . indexValidator

-------------------------------------------------------------------------------
-- Tally Nft Minter
-------------------------------------------------------------------------------

{-# INLINABLE integerToByteString #-}
integerToByteString :: Integer -> BuiltinByteString
integerToByteString n
  | n == 0 = "0"
  | n == 1 = "1"
  | n == 2 = "2"
  | n == 3 = "3"
  | n == 4 = "4"
  | n == 5 = "5"
  | n == 6 = "6"
  | n == 7 = "7"
  | n == 8 = "8"
  | n == 9 = "9"
  | otherwise
    =  integerToByteString (n `divide` 10)
    <> integerToByteString (n `modulo` 10)


data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId        :: CurrencySymbol
  , tncIndexNftTokenName       :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName      :: TokenName
  }

makeLift ''TallyNftConfig

mkTallyNftMinter :: TallyNftConfig -> BuiltinData -> ScriptContext -> Bool
mkTallyNftMinter TallyNftConfig {..} _ ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } =
  let
    hasConfigurationNft :: Value -> Bool
    hasConfigurationNft (Value v) = case M.lookup tncConfigNftCurrencySymbol v of
      Nothing -> False
      Just m  -> case M.lookup tncConfigNftTokenName m of
        Nothing -> False
        Just c -> c == 1

    DynamicConfig {..} = case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
      [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
      _ -> traceError "Too many Config NFT values"

    hasTallyNft :: Value -> Bool
    hasTallyNft (Value v) = case M.lookup thisCurrencySymbol v of
      Nothing -> False
      Just m  -> case M.lookup theTokenName m of
        Nothing -> False
        Just c | c == 1 -> True
               | otherwise -> traceError "wrong nft count"

    TxOut { txOutDatum = outputDatum, txOutAddress = outputAddress } = case filter (hasTallyNft . txOutValue) txInfoOutputs of
      [x] -> x
      _ -> traceError "wrong number of outputs"

    TallyState {..} = unsafeFromBuiltinData $ case outputDatum of
      OutputDatum (Datum dbs) -> dbs
      OutputDatumHash dh -> case M.lookup dh txInfoData of
        Just (Datum dbs) -> dbs
        _ -> traceError "Missing datum"
      NoOutputDatum -> traceError "Script input missing datum hash"

    hasIndexNft :: Value -> Bool
    hasIndexNft (Value v) = case M.lookup tncIndexNftPolicyId v of
      Nothing -> False
      Just m  -> case M.lookup tncIndexNftTokenName m of
        Nothing -> False
        Just c -> c == 1

    IndexNftDatum {..} = case filter (hasIndexNft . txOutValue . txInInfoResolved) txInfoInputs of
      [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
      _ -> traceError "Too many Index NFT values"

    theTokenName :: TokenName
    !theTokenName = TokenName (integerToByteString indIndex)

    onlyOneTokenMinted :: Bool
    !onlyOneTokenMinted =
      hasSingleToken
        txInfoMint
        thisCurrencySymbol
        theTokenName

    tallyIsInitializeToZero :: Bool
    !tallyIsInitializeToZero = tsFor == 0 && tsAgainst == 0

    outputOnTallyValidator :: Bool
    !outputOnTallyValidator = addressCredential outputAddress == ScriptCredential dcTallyValidator

  in traceIfFalse "Token is not on tally validator" outputOnTallyValidator
  && traceIfFalse "Tally datum is not initialized to zero" tallyIsInitializeToZero
  && traceIfFalse "Not only one token was minted" onlyOneTokenMinted

mkTallyNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicyTally :: TallyNftConfig -> WrappedMintingPolicyType
wrappedPolicyTally config a b = check (mkTallyNftMinter config a (unsafeFromBuiltinData b))

tallyNftPolicy :: TallyNftConfig -> MintingPolicy
tallyNftPolicy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicyTally c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

tallyNftPlutusScript :: TallyNftConfig -> Script
tallyNftPlutusScript = unMintingPolicyScript . tallyNftPolicy

tallyNftValidator :: TallyNftConfig -> Validator
tallyNftValidator = Validator . tallyNftPlutusScript

tallyNftMinterPolicyId :: TallyNftConfig -> CurrencySymbol
tallyNftMinterPolicyId = mpsSymbol . mintingPolicyHash . tallyNftPolicy

tallyNftScriptAsCbor :: TallyNftConfig -> BSL.ByteString
tallyNftScriptAsCbor = serialise . tallyNftValidator

tallyNftMinter :: TallyNftConfig -> PlutusScript PlutusScriptV2
tallyNftMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . tallyNftScriptAsCbor

-------------------------------------------------------------------------------
-- Tally Validator
-------------------------------------------------------------------------------

data TallyAddress = TallyAddress
  { cAddressCredential        :: Credential
  , cAddressStakingCredential :: BuiltinData
  }

data TallyTxOut = TallyTxOut
  { cTxOutAddress             :: TallyAddress
  , cTxOutValue               :: Value
  , cTxOutDatum               :: OutputDatum
  , cTxOutReferenceScript     :: BuiltinData
  }

data TallyTxInInfo = TallyTxInInfo
  { cTxInInfoOutRef   :: TxOutRef
  , cTxInInfoResolved :: TallyTxOut
  }

data TallyScriptPurpose = TallySpend TxOutRef

data TallyScriptContext = TallyScriptContext
  { cScriptContextTxInfo  :: TallyTxInfo
  , cScriptContextPurpose :: TallyScriptPurpose
  }

data TallyTxInfo = TallyTxInfo
  { cTxInfoInputs             :: [TallyTxInInfo]
  , cTxInfoReferenceInputs    :: [TallyTxInInfo]
  , cTxInfoOutputs            :: [TallyTxOut]
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

data TallyValidatorConfig = TallyValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName      :: TokenName
  }

unstableMakeIsData ''TallyAddress
unstableMakeIsData ''TallyTxOut
unstableMakeIsData ''TallyTxInInfo
makeIsDataIndexed  ''TallyScriptPurpose [('TallySpend,1)]
unstableMakeIsData ''TallyScriptContext
unstableMakeIsData ''TallyTxInfo
makeLift ''TallyValidatorConfig

ownValue :: [TallyTxInInfo] -> TxOutRef -> Value
ownValue ins txOutRef = go ins where
  go = \case
    [] -> traceError "The impossible happened"
    TallyTxInInfo {cTxInInfoOutRef, cTxInInfoResolved = TallyTxOut{cTxOutValue}} :xs ->
      if cTxInInfoOutRef == txOutRef then
        cTxOutValue
      else
        go xs

validateTally
  :: TallyValidatorConfig
  -> DynamicConfig
  -> BuiltinData
  -> TallyScriptContext
  -> Bool
validateTally
  TallyValidatorConfig {}
  DynamicConfig {}
  _
  TallyScriptContext
    { cScriptContextTxInfo = TallyTxInfo {}
    , cScriptContextPurpose = TallySpend _thisOutRef
    } = error ()

wrapValidateTally
    :: TallyValidatorConfig
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wrapValidateTally cfg x y z = check (
  validateTally
    cfg
    (unsafeFromBuiltinData x)
    (unsafeFromBuiltinData y)
    (unsafeFromBuiltinData z) )

tallyValidator :: TallyValidatorConfig -> Validator
tallyValidator cfg = let
    optimizerSettings = Plutonomy.defaultOptimizerOptions
      { Plutonomy.ooSplitDelay = False
      }
  in Plutonomy.optimizeUPLCWith optimizerSettings $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapValidateTally ||])
    `applyCode`
    liftCode cfg

tallyValidatorHash :: TallyValidatorConfig -> ValidatorHash
tallyValidatorHash = validatorHash . tallyValidator

tallyScript :: TallyValidatorConfig ->  PlutusScript PlutusScriptV2
tallyScript
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . tallyValidator
