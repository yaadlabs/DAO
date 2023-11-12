{- |
Module: Triphut.Tally.Script
Description: Triphut tally related scripts.
Includes:
  - Tally minting policy script.
  - Tally validator script.
-}
module Triphut.Tally.Script (
  tallyNftMinter,
  tallyNftMinterPolicyId,
  mkTallyNftMinter,
  tallyScript,
  tallyValidator,
  tallyValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript, PlutusScriptV2)
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Scripts (
  MintingPolicy,
  Validator,
  ValidatorHash,
  mkMintingPolicyScript,
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
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  integerToByteString,
  isScriptCredential,
  mintingPolicyHash,
  mkValidatorWithSettings,
  policyToScript,
  validatorHash,
  validatorToScript,
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
  DynamicConfigDatum (DynamicConfigDatum, dcTallyValidator),
  TallyStateDatum (TallyStateDatum, tsAgainst, tsFor, tsProposalEndTime),
 )
import Triphut.Vote (
  Vote (Vote, vDirection, vOwner, vProposalTokenName, vReturnAda),
  VoteDirection (For),
 )

{- | Policy for minting the Tally NFT.

   This policy performs the following checks:

    - There is exactly one 'DynamicConfigDatum' in the reference inputs,
      marked by the config NFT
      (Corresponding config 'CurrencySymbol' and 'TokenName' provided by the 'TallyNftConfig' argument)
    - There is exactly one Index UTXO spent (contained in the 'txInfoInputs')
    - This index UTXO contains a valid 'IndexNftDatum'
      (The 'Triphut.Index.Script.validateIndex' validator ensures the datum's index is incremented by one)
    - Exactly one valid Tally NFT is minted with the valid token name.
    - The token name matches the 'indIndex' field of the 'IndexNftDatum'
    - There is exactly one output containing the tally NFT.
    - This output contains a valid 'Triphut.Types.TallyStateDatum' datum.
    - The initial votes for fields of the 'Triphut.Types.TallyStateDatum' are both set to zero.
    - The tally output is at the tally validator
      (Corresponding to the tally script provided by the 'dcTallyValidator'
       field of the 'Triphut.Types.DynamicConfigDatum')
-}
mkTallyNftMinter :: TallyNftConfig -> BuiltinData -> ScriptContext -> Bool
mkTallyNftMinter
  TallyNftConfig {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      -- Helper for filtering for config UTXO in the reference inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken tncConfigNftCurrencySymbol tncConfigNftTokenName

      -- Get the configuration from the reference inputs
      DynamicConfigDatum {dcTallyValidator} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one valid config in the reference inputs"

      -- Helper for filtering for index UTXO in the inputs
      hasIndexNft :: Value -> Bool
      hasIndexNft = hasOneOfToken tncIndexNftPolicyId tncIndexNftTokenName

      -- Get the index datum from the inputs
      IndexNftDatum {indIndex} = case filter (hasIndexNft . txOutValue . txInInfoResolved) txInfoInputs of
        [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Should be exactly one valid Index NFT output"

      -- Helper for filtering for tally UTXO in the outputs
      hasTallyNft :: Value -> Bool
      hasTallyNft = hasOneOfToken thisCurrencySymbol theTokenName

      -- Get the tally state datum at the output marked by the tally NFT
      TxOut {txOutDatum = outputDatum, txOutAddress = outputAddress} =
        case filter (hasTallyNft . txOutValue) txInfoOutputs of
          [tallyTxOut] -> tallyTxOut
          _ -> traceError "Should be exactly one valid Tally NFT output"

      -- Unwrap the 'OutputDatum' to get the 'TallyStateDatum'
      -- Will fail with error if no datum found
      tallyStateDatum :: TallyStateDatum
      tallyStateDatum = convertDatum txInfoData outputDatum

      -- The initial votes for and against must both be set to zero
      tallyIsInitializeToZero :: Bool
      !tallyIsInitializeToZero = tsFor tallyStateDatum == 0 && tsAgainst tallyStateDatum == 0

      -- The NFT must be at the address of the tally validator
      outputOnTallyValidator :: Bool
      !outputOnTallyValidator = addressCredential outputAddress == ScriptCredential dcTallyValidator

      -- The token name be set to the index value,
      -- contained in the 'IndexNftDatum' ("0" initially)
      theTokenName :: TokenName
      !theTokenName = TokenName $ integerToByteString indIndex

      -- Ensure exactly one valid tally token is minted
      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          theTokenName
     in
      traceIfFalse "Tally NFT must be sent to the Tally validator" outputOnTallyValidator
        && traceIfFalse "Tally datum is not initialized to zero" tallyIsInitializeToZero
        && traceIfFalse "Should be exactly one valid token minted" onlyOneTokenMinted
mkTallyNftMinter _ _ _ = traceError "Wrong type of script purpose!"

wrappedPolicyTally :: TallyNftConfig -> WrappedMintingPolicyType
wrappedPolicyTally config a b = check (mkTallyNftMinter config a (unsafeFromBuiltinData b))

tallyNftPolicy :: TallyNftConfig -> MintingPolicy
tallyNftPolicy cfg =
  mkMintingPolicyScript $
    $$(compile [||\c -> wrappedPolicyTally c||])
      `PlutusTx.applyCode` PlutusTx.liftCode cfg

tallyNftMinterPolicyId :: TallyNftConfig -> CurrencySymbol
tallyNftMinterPolicyId = mpsSymbol . mintingPolicyHash . tallyNftPolicy

tallyNftMinter :: TallyNftConfig -> PlutusScript PlutusScriptV2
tallyNftMinter = policyToScript tallyNftPolicy

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
  TallyStateDatum ->
  BuiltinData ->
  TallyScriptContext ->
  Bool
validateTally
  TallyValidatorConfig {..}
  ts@TallyStateDatum {tsFor = oldFor, tsAgainst = oldAgainst, tsProposalEndTime}
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

      (!newValue, !newDatum) :: (Value, TallyStateDatum) =
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

tallyValidator :: TallyValidatorConfig -> Validator
tallyValidator config = mkValidatorWithSettings compiledCode False
  where
    wrapValidateTally = wrapValidate validateTally
    compiledCode = $$(PlutusTx.compile [||wrapValidateTally||]) `applyCode` liftCode config

tallyValidatorHash :: TallyValidatorConfig -> ValidatorHash
tallyValidatorHash = validatorHash . tallyValidator

tallyScript :: TallyValidatorConfig -> PlutusScript PlutusScriptV2
tallyScript = validatorToScript tallyValidator
