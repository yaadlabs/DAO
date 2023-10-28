module Canonical.Tally (
  IndexValidatorConfig (..),
  IndexNftConfig (..),
  TallyNftConfig (..),
  TallyValidatorConfig (..),
  indexScript,
  indexValidatorHash,
  tallyIndexNftMinter,
  tallyIndexNftMinterPolicyId,
  tallyNftMinter,
  tallyNftMinterPolicyId,
  tallyScript,
  tallyValidatorHash,
) where

import Canonical.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  hasSingleToken,
  mintingPolicyHash,
  validatorHash,
 )
import Canonical.Types (
  DynamicConfig (DynamicConfig, dcTallyValidator),
  TallyState (TallyState, tsAgainst, tsFor, tsProposalEndTime),
 )
import Canonical.Vote (
  Vote (Vote, vDirection, vOwner, vProposalTokenName, vReturnAda),
  VoteDirection (For),
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash,
  MintingPolicy,
  Script,
  Validator (Validator),
  ValidatorHash,
  mkMintingPolicyScript,
  unMintingPolicyScript,
 )
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value as V
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting, Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoData, txInfoInputs, txInfoMint, txInfoOutputs, txInfoReferenceInputs),
  findTxInByTxOutRef,
  getContinuingOutputs,
 )
import Plutus.V2.Ledger.Tx (
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
 )
import PlutusTx (
  applyCode,
  compile,
  liftCode,
  makeIsDataIndexed,
  makeLift,
  unsafeFromBuiltinData,
  unstableMakeIsData,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinByteString,
  BuiltinData,
  Integer,
  Maybe (Just, Nothing),
  all,
  any,
  check,
  divide,
  filter,
  foldr,
  length,
  map,
  mempty,
  modulo,
  not,
  otherwise,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (*),
  (+),
  (.),
  (<>),
  (==),
  (||),
 )
import PlutusTx.Prelude qualified as PlutusTx

data IndexNftDatum = IndexNftDatum
  { indIndex :: Integer
  }

data IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TxOutRef
  , incTokenName :: TokenName
  , incIndexValidator :: ValidatorHash
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig

mkIndexNftMinter :: IndexNftConfig -> BuiltinData -> ScriptContext -> Bool
mkIndexNftMinter
  IndexNftConfig {..}
  _
  ScriptContext
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

      (!IndexNftDatum {..}, !outputAddress) = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
        [TxOut {..}] -> (convertDatum txInfoData txOutDatum, txOutAddress)
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
     in
      traceIfFalse "Missing significant UTxO!" hasUTxO
        && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted
        && traceIfFalse "Initial Index is not zero" initialIndexIsZero
        && traceIfFalse "Output is not index validator" outputIsValidator
mkIndexNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: IndexNftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkIndexNftMinter config a (unsafeFromBuiltinData b))

policy :: IndexNftConfig -> MintingPolicy
policy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicy c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

plutusScript :: IndexNftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: IndexNftConfig -> Validator
validator = Validator . plutusScript

tallyIndexNftMinterPolicyId :: IndexNftConfig -> CurrencySymbol
tallyIndexNftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: IndexNftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

tallyIndexNftMinter :: IndexNftConfig -> PlutusScript PlutusScriptV2
tallyIndexNftMinter =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . scriptAsCbor

-------------------------------------------------------------------------------
-- Nft Index Validator
-------------------------------------------------------------------------------

data IndexValidatorConfig = IndexValidatorConfig
  { ivcConfigNftCurrencySymbol :: CurrencySymbol
  , ivcConfigNftTokenName :: TokenName
  , ivcNonce :: Integer -- to help with testing
  }

makeLift ''IndexValidatorConfig

validateIndex ::
  IndexValidatorConfig ->
  IndexNftDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateIndex
  IndexValidatorConfig {..}
  IndexNftDatum {indIndex = inputIndex}
  _
  ctx@ScriptContext
    { scriptContextTxInfo = info@TxInfo {..}
    , scriptContextPurpose = Spending thisOutRef
    } =
    let
      scriptValue :: Value
      !scriptValue = case findTxInByTxOutRef thisOutRef info of
        Nothing -> traceError "Impossible not input"
        Just TxInInfo {txInInfoResolved = TxOut {..}} -> txOutValue

      outputValue :: Value
      (!outputValue, !IndexNftDatum {indIndex = outputIndex}) = case getContinuingOutputs ctx of
        [TxOut {..}] -> (txOutValue, convertDatum txInfoData txOutDatum)
        _ -> traceError "wrong number of continuing outputs"

      outputValueGreaterThanInputValue :: Bool
      outputValueGreaterThanInputValue = outputValue `geq` scriptValue

      outputDatumIsIncremented :: Bool
      outputDatumIsIncremented = outputIndex == inputIndex + 1
     in
      traceIfFalse "output datum is not incremented" outputDatumIsIncremented
        && traceIfFalse "script value is not returned" outputValueGreaterThanInputValue
        && ivcNonce == ivcNonce -- to help with testing
validateIndex _ _ _ _ = traceError "Wrong script purpose"

wrapValidateIndex ::
  IndexValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidateIndex cfg x y z =
  check
    ( validateIndex
        cfg
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

indexValidator :: IndexValidatorConfig -> Validator
indexValidator cfg =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        }
   in
    Plutonomy.optimizeUPLCWith optimizerSettings $
      Plutonomy.validatorToPlutus $
        Plutonomy.mkValidatorScript $
          $$(PlutusTx.compile [||wrapValidateIndex||])
            `applyCode` liftCode cfg

indexValidatorHash :: IndexValidatorConfig -> ValidatorHash
indexValidatorHash = validatorHash . indexValidator

indexScript :: IndexValidatorConfig -> PlutusScript PlutusScriptV2
indexScript =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . indexValidator

-------------------------------------------------------------------------------
-- Tally Nft Minter
-------------------------------------------------------------------------------

{-# INLINEABLE integerToByteString #-}
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
  | otherwise =
      integerToByteString (n `divide` 10)
        <> integerToByteString (n `modulo` 10)

data TallyNftConfig = TallyNftConfig
  { tncIndexNftPolicyId :: CurrencySymbol
  , tncIndexNftTokenName :: TokenName
  , tncConfigNftCurrencySymbol :: CurrencySymbol
  , tncConfigNftTokenName :: TokenName
  }

makeLift ''TallyNftConfig

mkTallyNftMinter :: TallyNftConfig -> BuiltinData -> ScriptContext -> Bool
mkTallyNftMinter
  TallyNftConfig {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft (Value v) = case M.lookup tncConfigNftCurrencySymbol v of
        Nothing -> False
        Just m -> case M.lookup tncConfigNftTokenName m of
          Nothing -> False
          Just c -> c == 1

      DynamicConfig {..} = case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
        [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Too many Config NFT values"

      hasTallyNft :: Value -> Bool
      hasTallyNft (Value v) = case M.lookup thisCurrencySymbol v of
        Nothing -> False
        Just m -> case M.lookup theTokenName m of
          Nothing -> False
          Just c
            | c == 1 -> True
            | otherwise -> traceError "wrong nft count"

      TxOut {txOutDatum = outputDatum, txOutAddress = outputAddress} = case filter (hasTallyNft . txOutValue) txInfoOutputs of
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
        Just m -> case M.lookup tncIndexNftTokenName m of
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
     in
      traceIfFalse "Token is not on tally validator" outputOnTallyValidator
        && traceIfFalse "Tally datum is not initialized to zero" tallyIsInitializeToZero
        && traceIfFalse "Not only one token was minted" onlyOneTokenMinted
mkTallyNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicyTally :: TallyNftConfig -> WrappedMintingPolicyType
wrappedPolicyTally config a b = check (mkTallyNftMinter config a (unsafeFromBuiltinData b))

tallyNftPolicy :: TallyNftConfig -> MintingPolicy
tallyNftPolicy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicyTally c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

tallyNftPlutusScript :: TallyNftConfig -> Script
tallyNftPlutusScript = unMintingPolicyScript . tallyNftPolicy

tallyNftValidator :: TallyNftConfig -> Validator
tallyNftValidator = Validator . tallyNftPlutusScript

tallyNftMinterPolicyId :: TallyNftConfig -> CurrencySymbol
tallyNftMinterPolicyId = mpsSymbol . mintingPolicyHash . tallyNftPolicy

tallyNftScriptAsCbor :: TallyNftConfig -> BSL.ByteString
tallyNftScriptAsCbor = serialise . tallyNftValidator

tallyNftMinter :: TallyNftConfig -> PlutusScript PlutusScriptV2
tallyNftMinter =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . tallyNftScriptAsCbor

-------------------------------------------------------------------------------
-- Tally Validator
-------------------------------------------------------------------------------

data TallyTxOut = TallyTxOut
  { tTxOutAddress :: Address
  , tTxOutValue :: Value
  , tTxOutDatum :: OutputDatum
  , tTxOutReferenceScript :: BuiltinData
  }

data TallyTxInInfo = TallyTxInInfo
  { tTxInInfoOutRef :: TxOutRef
  , tTxInInfoResolved :: TallyTxOut
  }

data TallyScriptPurpose = TallySpend TxOutRef

data TallyScriptContext = TallyScriptContext
  { tScriptContextTxInfo :: TallyTxInfo
  , tScriptContextPurpose :: TallyScriptPurpose
  }

data TallyTxInfo = TallyTxInfo
  { tTxInfoInputs :: [TallyTxInInfo]
  , tTxInfoReferenceInputs :: [TallyTxInInfo]
  , tTxInfoOutputs :: [TallyTxOut]
  , tTxInfoFee :: BuiltinData
  , tTxInfoMint :: BuiltinData
  , tTxInfoDCert :: BuiltinData
  , tTxInfoWdrl :: BuiltinData
  , tTxInfoValidRange :: POSIXTimeRange
  , tTxInfoSignatories :: BuiltinData
  , tTxInfoRedeemers :: BuiltinData
  , tTxInfoData :: Map DatumHash Datum
  , tTxInfoId :: BuiltinData
  }

data TallyDynamicConfig = TallyDynamicConfig
  { tdcTallyIndexNft :: BuiltinData
  , tdcTallyNft :: CurrencySymbol
  , tdcTallyValidator :: BuiltinData
  , tdcTreasuryValidator :: BuiltinData
  , tdcConfigurationValidator :: BuiltinData
  , tdcVoteCurrencySymbol :: CurrencySymbol
  , tdcVoteTokenName :: BuiltinData
  , tdcVoteValidator :: ValidatorHash
  , tdcUpgradeMajorityPercent :: BuiltinData
  , tdcUpgradRelativeMajorityPercent :: BuiltinData
  , tdcGeneralMajorityPercent :: BuiltinData
  , tdcGeneralRelativeMajorityPercent :: BuiltinData
  , tdcTripMajorityPercent :: BuiltinData
  , tdcTripRelativeMajorityPercent :: BuiltinData
  , tdcTotalVotes :: BuiltinData
  , tdcVoteNft :: CurrencySymbol
  , tdcVoteFungibleCurrencySymbol :: CurrencySymbol
  , tdcVoteFungibleTokenName :: TokenName
  , tdcProposalTallyEndOffset :: BuiltinData
  , tdcMaxGeneralDisbursement :: BuiltinData
  , tdcMaxTripDisbursement :: BuiltinData
  , tdcAgentDisbursementPercent :: BuiltinData
  , tdcFungibleVotePercent :: Integer
  }

unstableMakeIsData ''TallyDynamicConfig

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

data TallyValidatorConfig = TallyValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''TallyTxOut
unstableMakeIsData ''TallyTxInInfo
makeIsDataIndexed ''TallyScriptPurpose [('TallySpend, 1)]
unstableMakeIsData ''TallyScriptContext
unstableMakeIsData ''TallyTxInfo
makeLift ''TallyValidatorConfig

ownValueAndValidator :: [TallyTxInInfo] -> TxOutRef -> (Value, ValidatorHash)
ownValueAndValidator ins txOutRef = go ins
  where
    go = \case
      [] -> traceError "The impossible happened"
      TallyTxInInfo {tTxInInfoOutRef, tTxInInfoResolved = TallyTxOut {tTxOutAddress = Address {..}, ..}} : xs ->
        if tTxInInfoOutRef == txOutRef
          then case addressCredential of
            ScriptCredential vh -> (tTxOutValue, vh)
            _ -> traceError "Impossible. Expected ScriptCredential"
          else go xs

isScriptCredential :: Credential -> Bool
isScriptCredential = \case
  ScriptCredential _ -> True
  _ -> False

hasExpectedScripts :: [TallyTxInInfo] -> ValidatorHash -> ValidatorHash -> Bool
hasExpectedScripts theInputs theTallyValidator voteValidator =
  let
    tallyCredential :: Credential
    !tallyCredential = ScriptCredential theTallyValidator

    voteCredential :: Credential
    !voteCredential = ScriptCredential voteValidator

    inputCredentials :: [Credential]
    inputCredentials =
      filter
        isScriptCredential
        (map (addressCredential . tTxOutAddress . tTxInInfoResolved) theInputs)

    onlyTallyOrVote :: Bool
    onlyTallyOrVote =
      all (\x -> tallyCredential == x || voteCredential == x) inputCredentials

    onlyOneTallyScript :: Bool
    onlyOneTallyScript =
      length (filter (== tallyCredential) inputCredentials) == 1
   in
    traceIfFalse "More than one tally input" onlyOneTallyScript
      && traceIfFalse "Invalid script inputs" onlyTallyOrVote

mapInsertWith :: PlutusTx.Eq k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
mapInsertWith f k v xs = case M.lookup k xs of
  Nothing -> M.insert k v xs
  Just v' -> M.insert k (f v v') xs

mergePayouts :: Address -> Value -> Map Address Value -> Map Address Value
mergePayouts addr value =
  mapInsertWith (<>) addr value

-- Optimize this to accum a value
valuePaidTo' :: [TallyTxOut] -> Address -> Value
valuePaidTo' outs addr = go mempty outs
  where
    go acc [] = acc
    go acc (TallyTxOut {tTxOutAddress, tTxOutValue} : xs)
      | addr == tTxOutAddress = go (acc <> tTxOutValue) xs
      | otherwise = go acc xs

validateTally ::
  TallyValidatorConfig ->
  TallyState ->
  BuiltinData ->
  TallyScriptContext ->
  Bool
validateTally
  TallyValidatorConfig {..}
  ts@TallyState {tsFor = oldFor, tsAgainst = oldAgainst, tsProposalEndTime}
  _
  TallyScriptContext
    { tScriptContextTxInfo = TallyTxInfo {..}
    , tScriptContextPurpose = TallySpend thisOutRef
    } =
    let
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft (Value v) = case M.lookup tvcConfigNftCurrencySymbol v of
        Nothing -> False
        Just m -> case M.lookup tvcConfigNftTokenName m of
          Nothing -> False
          Just c -> c == 1

      TallyDynamicConfig {..} = case filter (hasConfigurationNft . tTxOutValue . tTxInInfoResolved) tTxInfoReferenceInputs of
        [TallyTxInInfo {tTxInInfoResolved = TallyTxOut {..}}] -> convertDatum tTxInfoData tTxOutDatum
        _ -> traceError "Too many NFT values"

      oldValue :: Value
      thisValidatorHash :: ValidatorHash

      (!oldValue, !thisValidatorHash) = ownValueAndValidator tTxInfoInputs thisOutRef
      -- Make sure there is only one tally and many votes
      expectedScripts :: Bool
      !expectedScripts = hasExpectedScripts tTxInfoInputs thisValidatorHash tdcVoteValidator

      hasVoteToken :: Value -> Maybe Value
      hasVoteToken (Value v) =
        case filter (\(k, _) -> tdcVoteNft == k) (M.toList v) of
          [] -> Nothing
          xs@[_] -> Just (Value (M.fromList xs))
          _ -> traceError "Too many vote nfts"

      hasVoteWitness :: Value -> Bool
      hasVoteWitness (Value v) = case M.lookup tdcVoteCurrencySymbol v of
        Nothing -> False
        Just _ -> True

      thisTallyTokenName :: TokenName
      !thisTallyTokenName = case M.lookup tdcTallyNft (getValue oldValue) of
        Nothing -> traceError "Failed to find tally nft"
        Just m -> case M.toList m of
          [(t, c)]
            | c == 1 -> t
            | otherwise -> traceError "bad tally nft count"
          _ -> traceError "wrong number of tally nfts"

      stepVotes ::
        TallyTxInInfo ->
        (Integer, Integer, Map Address Value) ->
        (Integer, Integer, Map Address Value)
      stepVotes TallyTxInInfo {tTxInInfoResolved = TallyTxOut {..}} oldAcc@(oldForCount, oldAgainstCount, oldPayoutMap) =
        case (hasVoteToken tTxOutValue, hasVoteWitness tTxOutValue) of
          (Just voteNft, True) ->
            let
              Vote {..} = convertDatum tTxInfoData tTxOutDatum

              -- Count all the dcVoteFungibleCurrencySymbol with dcVoteFungibleTokenName tokens on the vote utxo
              fungibleTokens :: Integer
              !fungibleTokens = case M.lookup tdcVoteFungibleCurrencySymbol (getValue tTxOutValue) of
                Nothing -> 0
                Just m -> case M.lookup tdcVoteFungibleTokenName m of
                  Nothing -> 0
                  Just c -> c

              -- Calculate fungible votes using the dcFungibleVotePercent
              fungibleVotes :: Integer
              !fungibleVotes = if fungibleTokens == 0 then 0 else (fungibleTokens * tdcFungibleVotePercent) `divide` 1000

              -- Add the lovelaces and the NFT
              votePayout :: Value
              !votePayout =
                if fungibleTokens == 0
                  then Value (M.insert adaSymbol (M.singleton adaToken vReturnAda) (getValue voteNft))
                  else Value (M.insert tdcVoteFungibleCurrencySymbol (M.singleton tdcVoteFungibleTokenName fungibleTokens) (M.insert adaSymbol (M.singleton adaToken vReturnAda) (getValue voteNft)))

              checkProposal :: Bool
              !checkProposal = vProposalTokenName == thisTallyTokenName

              newForCount :: Integer
              !newForCount = oldForCount + if vDirection == For then 1 + fungibleVotes else 0

              newAgainstCount :: Integer
              !newAgainstCount = oldAgainstCount + if vDirection == For then 0 else 1 + fungibleVotes

              newPayoutMap :: Map Address Value
              !newPayoutMap = mergePayouts vOwner votePayout oldPayoutMap
             in
              if checkProposal
                then (newForCount, newAgainstCount, newPayoutMap)
                else traceError "wrong vote proposal"
          _ -> oldAcc

      -- Collect the votes
      -- Make sure the votes are for the write proposal
      -- Make sure the votes have the vote witness
      forCount :: Integer
      againstCount :: Integer
      payoutMap :: Map Address Value
      (!forCount, !againstCount, !payoutMap) = foldr stepVotes (0, 0, M.empty) tTxInfoInputs

      -- return the vote tokens to the owner
      addressedIsPaid :: [TallyTxOut] -> (Address, Value) -> Bool
      addressedIsPaid outputs (addr, value) = valuePaidTo' outputs addr `geq` value

      voteNftAndAdaToVoters :: Bool
      !voteNftAndAdaToVoters = all (addressedIsPaid tTxInfoOutputs) (M.toList payoutMap)

      tallyingIsInactive :: Bool
      !tallyingIsInactive = tsProposalEndTime `before` tTxInfoValidRange

      voteTokenAreAllBurned :: Bool
      !voteTokenAreAllBurned = not $ any (hasVoteWitness . tTxOutValue) tTxInfoOutputs

      newDatum :: TallyState
      newValue :: Value

      (!newValue, !newDatum) = case filter
        ( \TallyTxOut {tTxOutAddress = Address {..}} ->
            addressCredential == ScriptCredential thisValidatorHash
        )
        tTxInfoOutputs of
        [TallyTxOut {..}] -> (tTxOutValue, convertDatum tTxInfoData tTxOutDatum)
        _ -> traceError "Wrong number of continuing outputs"

      newValueIsAtleastAsBigAsOldValue :: Bool
      !newValueIsAtleastAsBigAsOldValue = newValue `geq` oldValue

      -- Tally datum is updated
      tallyDatumIsUpdated :: Bool
      !tallyDatumIsUpdated =
        newDatum
          == ts
            { tsFor = oldFor + forCount
            , tsAgainst = oldAgainst + againstCount
            }
     in
      traceIfFalse "Tally is active" tallyingIsInactive
        && traceIfFalse "Unexpected scripts" expectedScripts
        && traceIfFalse "Not all vote tokens and Ada returned" voteNftAndAdaToVoters
        && traceIfFalse "Not all vote tokens are burned" voteTokenAreAllBurned
        && traceIfFalse "Tally datum is not updated" tallyDatumIsUpdated
        && traceIfFalse "Old value is not as big as new value" newValueIsAtleastAsBigAsOldValue

wrapValidateTally ::
  TallyValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidateTally cfg x y z =
  check
    ( validateTally
        cfg
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

tallyValidator :: TallyValidatorConfig -> Validator
tallyValidator cfg =
  let
    optimizerSettings =
      Plutonomy.defaultOptimizerOptions
        { Plutonomy.ooSplitDelay = False
        , Plutonomy.ooFloatOutLambda = False
        }
   in
    Plutonomy.optimizeUPLCWith optimizerSettings $
      Plutonomy.validatorToPlutus $
        Plutonomy.mkValidatorScript $
          $$(PlutusTx.compile [||wrapValidateTally||])
            `applyCode` liftCode cfg

tallyValidatorHash :: TallyValidatorConfig -> ValidatorHash
tallyValidatorHash = validatorHash . tallyValidator

tallyScript :: TallyValidatorConfig -> PlutusScript PlutusScriptV2
tallyScript =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . tallyValidator
