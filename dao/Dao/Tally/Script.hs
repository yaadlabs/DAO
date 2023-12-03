{- |
Module: Dao.Tally.Script
Description: Dao tally related scripts. It includes:
  - Tally minting policy script.
  - Tally validator script.
-}
module Dao.Tally.Script (
  -- * Minting policy
  tallyNftMinter,
  tallyNftMinterPolicyId,
  mkTallyNftMinter,

  -- * Validator
  tallyScript,
  tallyValidator,
  tallyValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript, PlutusScriptV2)
import Dao.ConfigurationNft (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
 )
import Dao.Index (IndexNftDatum (IndexNftDatum, indIndex))
import Dao.Shared (
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
import Dao.Tally (
  TallyNftConfig (
    TallyNftConfig,
    tncConfigNftCurrencySymbol,
    tncConfigNftTokenName,
    tncIndexNftPolicyId,
    tncIndexNftTokenName
  ),
 )
import Dao.Types (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dcFungibleVotePercent,
    dcTallyNft,
    dcTallyValidator,
    dcVoteFungibleCurrencySymbol,
    dcVoteFungibleTokenName,
    dcVoteNft,
    dcVoteValidator
  ),
  TallyStateDatum (TallyStateDatum, tsAgainst, tsFor, tsProposalEndTime),
 )
import Dao.Vote (
  VoteDatum (VoteDatum, vDirection, vOwner, vProposalTokenName, vReturnAda),
  VoteDirection (For),
 )
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
  ScriptPurpose (Minting, Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoReferenceInputs,
    txInfoValidRange
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

{- | Policy for minting the Tally NFT.

   This policy performs the following checks:

    - There is exactly one 'DynamicConfigDatum' in the reference inputs,
      marked by the config NFT
      (Corresponding config 'CurrencySymbol' and 'TokenName' provided by the 'TallyNftConfig' argument)
    - There is exactly one Index UTXO spent (contained in the 'txInfoInputs')
    - This index UTXO contains a valid 'IndexNftDatum'
      (The 'Dao.Index.Script.validateIndex' validator ensures the datum's index is incremented by one)
    - Exactly one valid Tally NFT is minted with the valid token name.
    - The token name matches the 'indIndex' field of the 'IndexNftDatum'
    - There is exactly one output containing the tally NFT.
    - This output contains a valid 'Dao.Types.TallyStateDatum' datum.
    - The initial vote count fields `tsFor` and `tsAgainst` of
      the 'Dao.Types.TallyStateDatum' are both set to zero.
    - The tally output is at the tally validator
      (Corresponding to the tally script provided by the 'dcTallyValidator'
       field of the 'Dao.Types.DynamicConfigDatum')
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

      -- The token name must be set to the index value,
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
        && traceIfFalse "Tally datum vote counts are not initialized to zero" tallyIsInitializeToZero
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
ownValueAndValidator :: [TxInInfo] -> TxOutRef -> (Value, ValidatorHash)
ownValueAndValidator ins txOutRef = go ins
  where
    go = \case
      [] -> traceError "The impossible happened"
      TxInInfo {txInInfoOutRef, txInInfoResolved = TxOut {txOutAddress = Address {..}, ..}} : xs ->
        if txInInfoOutRef == txOutRef
          then case addressCredential of
            ScriptCredential vh -> (txOutValue, vh)
            _ -> traceError "Impossible. Expected ScriptCredential"
          else go xs

hasExpectedScripts :: [TxInInfo] -> ValidatorHash -> ValidatorHash -> Bool
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
        (map (addressCredential . txOutAddress . txInInfoResolved) theInputs)

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
valuePaidTo' :: [TxOut] -> Address -> Value
valuePaidTo' outs addr = go mempty outs
  where
    go acc [] = acc
    go acc (TxOut {txOutAddress, txOutValue} : xs)
      | addr == txOutAddress = go (acc <> txOutValue) xs
      | otherwise = go acc xs

{- | Validator for tallying.

  This validator performs the following checks:

    - There is exactly one 'Dao.Tally.DynamicConfigDatum' in the reference inputs,
      marked by the tally NFT. (Corresponding config 'CurrencySymbol' and 'TokenName'
      provided by the 'ConfigurationValidatorConfig' argument)

    - That the tally NFT remains at the validator (the 'newValueIsAtleastAsBigAsOldValue' check)

    - There is exactly one 'Dao.Tally.TallyStateDatum' in the outputs.

    - This 'Dao.Tally.TallyStateDatum' in the outpus has been updated accordingly.
      We check this by ensuring the the new votes have been added to the 'tsFor' and 'tsAgainst'
      vote count fields of the new tally datum at the output.

    - That the proposal period has passed. We do this by checking the 'tsProposalEndTime' field of
      the 'TallyStateDatum' against the transaction validity range, ensuring the proposal end time has passed.

    - That all vote tokens are burned (there are no vote tokens in the outputs).
-}
validateTally ::
  ConfigurationValidatorConfig ->
  TallyStateDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateTally
  ConfigurationValidatorConfig {..}
  ts@TallyStateDatum {tsFor = oldFor, tsAgainst = oldAgainst, tsProposalEndTime}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Spending thisOutRef
    } =
    let
      -- Helper for filtering for config UTXO in the reference inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken cvcConfigNftCurrencySymbol cvcConfigNftTokenName

      -- Get the 'DynamicConfig' from the reference inputs
      DynamicConfigDatum {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one tally NFT in the reference inputs"

      (!oldValue, !thisValidatorHash) :: (Value, ValidatorHash) = ownValueAndValidator txInfoInputs thisOutRef

      -- Make sure there is only one tally and many votes
      expectedScripts :: Bool
      !expectedScripts = hasExpectedScripts txInfoInputs thisValidatorHash dcVoteValidator

      hasVoteToken :: Value -> Maybe Value
      hasVoteToken (Value v) =
        case filter (\(k, _) -> dcVoteNft == k) (M.toList v) of
          [] -> Nothing
          xs@[_] -> Just (Value (M.fromList xs))
          _ -> traceError "Too many vote nfts"

      hasVoteWitness :: Value -> Bool
      hasVoteWitness = hasSymbolInValue dcVoteFungibleCurrencySymbol

      thisTallyTokenName :: TokenName
      !thisTallyTokenName = getTokenNameOfNft dcTallyNft oldValue "Tally Nft"

      -- Helper for loop that counts the votes
      stepVotes ::
        TxInInfo ->
        (Integer, Integer, Map Address Value) ->
        (Integer, Integer, Map Address Value)
      stepVotes
        TxInInfo {txInInfoResolved = TxOut {..}}
        oldAcc@(oldForCount, oldAgainstCount, oldPayoutMap) =
          case (hasVoteToken txOutValue, hasVoteWitness txOutValue) of
            (Just voteNft, True) ->
              let
                VoteDatum {..} = convertDatum txInfoData txOutDatum

                -- Count all the dcVoteFungibleCurrencySymbol
                -- with dcVoteFungibleTokenName tokens on the vote utxo
                fungibleTokens :: Integer
                !fungibleTokens =
                  countOfTokenInValue
                    dcVoteFungibleCurrencySymbol
                    dcVoteFungibleTokenName
                    txOutValue

                -- Calculate fungible votes using the dcFungibleVotePercent
                fungibleVotes :: Integer
                !fungibleVotes
                  | fungibleTokens == 0 = 0
                  | otherwise = (fungibleTokens * dcFungibleVotePercent) `divide` 1000

                -- Add the lovelaces and the NFT
                votePayout :: Value
                !votePayout =
                  if fungibleTokens == 0
                    then Value (M.insert adaSymbol (M.singleton adaToken vReturnAda) (getValue voteNft))
                    else
                      Value $
                        M.insert
                          dcVoteFungibleCurrencySymbol
                          (M.singleton dcVoteFungibleTokenName fungibleTokens)
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
        foldr stepVotes (0, 0, M.empty) txInfoInputs

      -- Helper for ensuring the vote NFT and ada are returned to the owner
      addressedIsPaid :: [TxOut] -> (Address, Value) -> Bool
      addressedIsPaid outputs (addr, value) = valuePaidTo' outputs addr `geq` value

      voteNftAndAdaToVoters :: Bool
      !voteNftAndAdaToVoters = all (addressedIsPaid txInfoOutputs) (M.toList payoutMap)

      tallyingIsInactive :: Bool
      !tallyingIsInactive = tsProposalEndTime `before` txInfoValidRange

      voteTokenAreAllBurned :: Bool
      !voteTokenAreAllBurned = not $ any (hasVoteWitness . txOutValue) txInfoOutputs

      (!newValue, !newDatum) :: (Value, TallyStateDatum) =
        case filter
          ( \TxOut {txOutAddress = Address {..}} ->
              addressCredential == ScriptCredential thisValidatorHash
          )
          txInfoOutputs of
          [TxOut {..}] -> (txOutValue, convertDatum txInfoData txOutDatum)
          _ -> traceError "Wrong number of continuing outputs"

      -- Ensure the tally NFT remains at the validator
      newValueIsAtleastAsBigAsOldValue :: Bool
      !newValueIsAtleastAsBigAsOldValue = newValue `geq` oldValue

      -- Ensure the tally datum is updated
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
validateTally _ _ _ _ = traceError "Wrong script purpose"

tallyValidator :: ConfigurationValidatorConfig -> Validator
tallyValidator config = mkValidatorWithSettings compiledCode False
  where
    wrapValidateTally = wrapValidate validateTally
    compiledCode = $$(PlutusTx.compile [||wrapValidateTally||]) `applyCode` liftCode config

tallyValidatorHash :: ConfigurationValidatorConfig -> ValidatorHash
tallyValidatorHash = validatorHash . tallyValidator

tallyScript :: ConfigurationValidatorConfig -> PlutusScript PlutusScriptV2
tallyScript = validatorToScript tallyValidator
