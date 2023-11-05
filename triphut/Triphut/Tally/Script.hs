module Triphut.Tally.Script (
  tallyNftMinter,
  tallyNftMinterPolicyId,
  tallyScript,
  tallyValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  Script,
  Validator (Validator),
  ValidatorHash,
  mkMintingPolicyScript,
  unMintingPolicyScript,
 )
import Plutus.V1.Ledger.Value (
  CurrencySymbol,
  TokenName (TokenName),
  Value (Value),
  adaSymbol,
  adaToken,
  geq,
  getValue,
  mpsSymbol,
 )
import Plutus.V2.Ledger.Contexts (
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxInInfo (TxInInfo, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoReferenceInputs
  ),
 )
import Plutus.V2.Ledger.Tx (
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
 )
import PlutusTx (
  applyCode,
  compile,
  liftCode,
  unsafeFromBuiltinData,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import PlutusTx.Prelude (
  Bool (False, True),
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
import Triphut.Index (IndexNftDatum (IndexNftDatum, indIndex))
import Triphut.Shared (
  WrappedMintingPolicyType,
  convertDatum,
  countOfTokenInValue,
  getTokenNameOfNft,
  hasOneOfToken,
  hasSingleToken,
  hasSymbolInValue,
  integerToByteString,
  isScriptCredential,
  mintingPolicyHash,
  validatorHash,
  wrapValidate,
 )
import Triphut.Tally (
  TallyDynamicConfig (
    TallyDynamicConfig,
    tdcFungibleVotePercent,
    tdcTallyNft,
    tdcVoteFungibleCurrencySymbol,
    tdcVoteFungibleTokenName,
    tdcVoteNft,
    tdcVoteValidator
  ),
  TallyNftConfig (
    TallyNftConfig,
    tncConfigNftCurrencySymbol,
    tncConfigNftTokenName,
    tncIndexNftPolicyId,
    tncIndexNftTokenName
  ),
  TallyScriptContext (
    TallyScriptContext,
    tScriptContextPurpose,
    tScriptContextTxInfo
  ),
  TallyScriptPurpose (TallySpend),
  TallyTxInInfo (
    TallyTxInInfo,
    tTxInInfoOutRef,
    tTxInInfoResolved
  ),
  TallyTxInfo (
    TallyTxInfo,
    tTxInfoData,
    tTxInfoInputs,
    tTxInfoOutputs,
    tTxInfoReferenceInputs,
    tTxInfoValidRange
  ),
  TallyTxOut (
    TallyTxOut,
    tTxOutAddress,
    tTxOutDatum,
    tTxOutValue
  ),
  TallyValidatorConfig (
    TallyValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
 )
import Triphut.Types (
  DynamicConfig (DynamicConfig, dcTallyValidator),
  TallyState (TallyState, tsAgainst, tsFor, tsProposalEndTime),
 )
import Triphut.Vote (
  Vote (Vote, vDirection, vOwner, vProposalTokenName, vReturnAda),
  VoteDirection (For),
 )

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
      hasConfigurationNft = hasOneOfToken tncConfigNftCurrencySymbol tncConfigNftTokenName

      DynamicConfig {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Too many Config NFT values"

      hasTallyNft :: Value -> Bool
      hasTallyNft = hasOneOfToken thisCurrencySymbol theTokenName

      TxOut {txOutDatum = outputDatum, txOutAddress = outputAddress} =
        case filter (hasTallyNft . txOutValue) txInfoOutputs of
          [x] -> x
          _ -> traceError "wrong number of outputs"

      TallyState {..} = convertDatum txInfoData outputDatum

      hasIndexNft :: Value -> Bool
      hasIndexNft = hasOneOfToken tncIndexNftPolicyId tncIndexNftTokenName

      IndexNftDatum {..} = case filter (hasIndexNft . txOutValue . txInInfoResolved) txInfoInputs of
        [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Too many Index NFT values"

      theTokenName :: TokenName
      !theTokenName = TokenName $ integerToByteString indIndex

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

-- | Validator
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
mergePayouts = mapInsertWith (<>)

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
      hasConfigurationNft = hasOneOfToken tvcConfigNftCurrencySymbol tvcConfigNftTokenName

      TallyDynamicConfig {..} =
        case filter (hasConfigurationNft . tTxOutValue . tTxInInfoResolved) tTxInfoReferenceInputs of
          [TallyTxInInfo {tTxInInfoResolved = TallyTxOut {..}}] -> convertDatum tTxInfoData tTxOutDatum
          _ -> traceError "Too many NFT values"

      (!oldValue, !thisValidatorHash) :: (Value, ValidatorHash) = ownValueAndValidator tTxInfoInputs thisOutRef

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
      hasVoteWitness = hasSymbolInValue tdcVoteFungibleCurrencySymbol

      thisTallyTokenName :: TokenName
      !thisTallyTokenName = getTokenNameOfNft tdcTallyNft oldValue "Tally Nft"

      stepVotes ::
        TallyTxInInfo ->
        (Integer, Integer, Map Address Value) ->
        (Integer, Integer, Map Address Value)
      stepVotes
        TallyTxInInfo {tTxInInfoResolved = TallyTxOut {..}}
        oldAcc@(oldForCount, oldAgainstCount, oldPayoutMap) =
          case (hasVoteToken tTxOutValue, hasVoteWitness tTxOutValue) of
            (Just voteNft, True) ->
              let
                Vote {..} = convertDatum tTxInfoData tTxOutDatum

                -- Count all the dcVoteFungibleCurrencySymbol
                -- with dcVoteFungibleTokenName tokens on the vote utxo
                fungibleTokens :: Integer
                !fungibleTokens =
                  countOfTokenInValue
                    tdcVoteFungibleCurrencySymbol
                    tdcVoteFungibleTokenName
                    tTxOutValue

                -- Calculate fungible votes using the dcFungibleVotePercent
                fungibleVotes :: Integer
                !fungibleVotes
                  | fungibleTokens == 0 = 0
                  | otherwise = (fungibleTokens * tdcFungibleVotePercent) `divide` 1000

                -- Add the lovelaces and the NFT
                votePayout :: Value
                !votePayout =
                  if fungibleTokens == 0
                    then Value (M.insert adaSymbol (M.singleton adaToken vReturnAda) (getValue voteNft))
                    else
                      Value $
                        M.insert
                          tdcVoteFungibleCurrencySymbol
                          (M.singleton tdcVoteFungibleTokenName fungibleTokens)
                          ( M.insert
                              adaSymbol
                              (M.singleton adaToken vReturnAda)
                              (getValue voteNft)
                          )

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
      -- Make sure the votes are for the right proposal
      -- Make sure the votes have the vote witness
      (!forCount, !againstCount, !payoutMap) :: (Integer, Integer, Map Address Value) =
        foldr stepVotes (0, 0, M.empty) tTxInfoInputs

      -- return the vote tokens to the owner
      addressedIsPaid :: [TallyTxOut] -> (Address, Value) -> Bool
      addressedIsPaid outputs (addr, value) = valuePaidTo' outputs addr `geq` value

      voteNftAndAdaToVoters :: Bool
      !voteNftAndAdaToVoters = all (addressedIsPaid tTxInfoOutputs) (M.toList payoutMap)

      tallyingIsInactive :: Bool
      !tallyingIsInactive = tsProposalEndTime `before` tTxInfoValidRange

      voteTokenAreAllBurned :: Bool
      !voteTokenAreAllBurned = not $ any (hasVoteWitness . tTxOutValue) tTxInfoOutputs

      (!newValue, !newDatum) :: (Value, TallyState) =
        case filter
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

wrapValidateTally :: TallyValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidateTally = wrapValidate validateTally

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
