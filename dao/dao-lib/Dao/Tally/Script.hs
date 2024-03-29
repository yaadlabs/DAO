{- |
Module: Dao.Tally.Script
Description: Dao tally related scripts. It includes:
  - Tally minting policy script.
  - Tally validator script.
-}
module Dao.Tally.Script (
  -- * Minting policy
  mkTallyNftMinter,
  tallyPolicyCompiledCode,

  -- * Validator
  tallyValidatorCompiledCode,
  validateTally,
) where

import Dao.ScriptArgument (
  TallyPolicyParams (
    TallyPolicyParams,
    tpConfigSymbol,
    tpConfigTokenName,
    tpIndexSymbol,
    tpIndexTokenName
  ),
  ValidatorParams (
    ValidatorParams,
    vpConfigSymbol,
    vpConfigTokenName
  ),
 )
import Dao.Shared (
  convertDatum,
  countOfTokenInValue,
  getTokenNameOfNft,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  integerToByteString,
  isScriptCredential,
  untypedPolicy,
  untypedValidator,
 )
import LambdaBuffers.ApplicationTypes.Configuration (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dynamicConfigDatum'fungibleVotePercent,
    dynamicConfigDatum'tallyNft,
    dynamicConfigDatum'tallyValidator,
    dynamicConfigDatum'voteCurrencySymbol,
    dynamicConfigDatum'voteFungibleCurrencySymbol,
    dynamicConfigDatum'voteFungibleTokenName,
    dynamicConfigDatum'voteNft,
    dynamicConfigDatum'voteTokenName,
    dynamicConfigDatum'voteValidator
  ),
 )
import LambdaBuffers.ApplicationTypes.Index (
  IndexDatum (IndexDatum, indexDatum'index),
 )
import LambdaBuffers.ApplicationTypes.Tally (
  TallyStateDatum (
    TallyStateDatum,
    tallyStateDatum'against,
    tallyStateDatum'for,
    tallyStateDatum'proposalEndTime
  ),
 )
import LambdaBuffers.ApplicationTypes.Vote (
  VoteDatum (
    VoteDatum,
    voteDatum'direction,
    voteDatum'proposalTokenName,
    voteDatum'returnAda,
    voteDatum'voteOwner
  ),
  VoteDirection (VoteDirection'For),
 )
import PlutusLedgerApi.V1.Address (Address (Address, addressCredential))
import PlutusLedgerApi.V1.Credential (Credential (ScriptCredential))
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Value (
  TokenName (TokenName),
  Value (Value),
  adaSymbol,
  adaToken,
  geq,
  getValue,
 )
import PlutusLedgerApi.V2.Contexts (
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
import PlutusLedgerApi.V2.Tx (
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
 )
import PlutusTx (
  CompiledCode,
  compile,
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as M
import PlutusTx.Prelude (
  Bool (True),
  BuiltinData,
  Integer,
  Maybe (Just, Nothing),
  all,
  any,
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
      (Corresponding config 'CurrencySymbol' and 'TokenName' provided by the 'TallyPolicyParams' argument)
    - There is exactly one Index UTXO spent (contained in the 'txInfoInputs')
    - This index UTXO contains a valid 'IndexDatum'
      (The 'Dao.Index.Script.validateIndex' validator ensures the datum's index is incremented by one)
    - Exactly one valid Tally NFT is minted with the valid token name.
    - The token name matches the 'indexDatum'index' field of the 'IndexDatum'
    - There is exactly one output containing the tally NFT.
    - This output contains a valid 'Dao.Types.TallyStateDatum' datum.
    - The initial vote count fields `tallyStateDatum'for` and `tallyStateDatum'against` of
      the 'Dao.Types.TallyStateDatum' are both set to zero.
    - The tally output is at the tally validator
      (Corresponding to the tally script provided by the 'dynamicConfigDatum'tallyValidator'
       field of the 'Dao.Types.DynamicConfigDatum')
-}
mkTallyNftMinter :: TallyPolicyParams -> BuiltinData -> ScriptContext -> Bool
mkTallyNftMinter
  TallyPolicyParams {..}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      -- Helper for filtering for config UTXO in the reference inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken tpConfigSymbol tpConfigTokenName

      -- Get the configuration from the reference inputs
      DynamicConfigDatum {dynamicConfigDatum'tallyValidator} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one valid config in the reference inputs"

      -- Helper for filtering for index UTXO in the inputs
      hasIndexNft :: Value -> Bool
      hasIndexNft = hasOneOfToken tpIndexSymbol tpIndexTokenName

      -- Get the index datum from the inputs
      IndexDatum {indexDatum'index} = case filter (hasIndexNft . txOutValue . txInInfoResolved) txInfoInputs of
        [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
        [] -> traceError "No index NFT found in inputs"
        _ -> traceError "Should be exactly one valid Index NFT output"

      -- Helper for filtering for tally UTXO in the outputs
      hasTallyNft :: Value -> Bool
      hasTallyNft = hasOneOfToken thisCurrencySymbol theTokenName

      -- Get the tally state datum at the output marked by the tally NFT
      TxOut {txOutDatum = outputDatum, txOutAddress = outputAddress} =
        case filter (hasTallyNft . txOutValue) txInfoOutputs of
          [tallyTxOut] -> tallyTxOut
          [] -> traceError "No tally NFT found in outputs"
          _ -> traceError "Should be exactly one valid Tally NFT output"

      -- Unwrap the 'OutputDatum' to get the 'TallyStateDatum'
      -- Will fail with error if no datum found
      tallyStateDatum :: TallyStateDatum
      tallyStateDatum = convertDatum txInfoData outputDatum

      -- The initial votes for and against must both be set to zero
      tallyIsInitializeToZero :: Bool
      !tallyIsInitializeToZero = tallyStateDatum'for tallyStateDatum == 0 && tallyStateDatum'against tallyStateDatum == 0

      -- The NFT must be at the address of the tally validator
      outputOnTallyValidator :: Bool
      !outputOnTallyValidator = addressCredential outputAddress == ScriptCredential dynamicConfigDatum'tallyValidator

      -- The token name must be set to the index value,
      -- contained in the 'IndexDatum' ("0" initially)
      theTokenName :: TokenName
      !theTokenName = TokenName $ integerToByteString indexDatum'index

      -- Ensure exactly one valid tally token is minted
      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          theTokenName
     in
      traceIfFalse "Tally datum vote counts are not initialized to zero" tallyIsInitializeToZero
        && traceIfFalse "Tally NFT must be sent to the Tally validator" outputOnTallyValidator
        && traceIfFalse "Should be exactly one valid token minted" onlyOneTokenMinted
mkTallyNftMinter _ _ _ = traceError "Wrong type of script purpose!"

untypedTallyPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTallyPolicy = untypedPolicy mkTallyNftMinter

tallyPolicyCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
tallyPolicyCompiledCode = $$(PlutusTx.compile [||untypedTallyPolicy||])

-- | Validator
ownValueAndValidator :: [TxInInfo] -> TxOutRef -> (Value, ScriptHash)
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

hasExpectedScripts :: [TxInInfo] -> ScriptHash -> ScriptHash -> Bool
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

mapInsertWith :: (PlutusTx.Eq k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
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

    - There is exactly one 'LambdaBuffers.ApplicationTypes.Configuration.DynamicConfigDatum' in the reference inputs,
      marked by the tally NFT. (Corresponding config 'CurrencySymbol' and 'TokenName'
      provided by the 'ValidatorParams' argument)

    - That the tally NFT remains at the validator (the 'newValueIsAtleastAsBigAsOldValue' check)

    - There is exactly one 'LambdaBuffers.ApplicationTypes.Tally.TallyStateDatum' in the outputs.

    - This 'TallyStateDatum' in the outputs has been updated accordingly.
      We check this by ensuring the the new votes have been added to the 'tallyStateDatum'for' and 'tallyStateDatum'against'
      vote count fields of the new tally datum at the output.

    - That the proposal period has passed. We do this by checking the 'tallyStateDatum'proposalEndTime' field of
      the 'TallyStateDatum' against the transaction validity range, ensuring the proposal end time has passed.

    - That all vote tokens are burned (there are no vote tokens in the outputs).
-}
validateTally ::
  ValidatorParams ->
  TallyStateDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateTally
  ValidatorParams {..}
  ts@TallyStateDatum {tallyStateDatum'for = oldFor, tallyStateDatum'against = oldAgainst, tallyStateDatum'proposalEndTime}
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Spending thisOutRef
    } =
    let
      -- Helper for filtering for config UTXO in the reference inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken vpConfigSymbol vpConfigTokenName

      -- Get the 'DynamicConfig' from the reference inputs
      DynamicConfigDatum {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one config NFT in the reference inputs"

      (!oldValue, !thisValidatorHash) :: (Value, ScriptHash) = ownValueAndValidator txInfoInputs thisOutRef

      -- Make sure there is only one tally and many votes
      expectedScripts :: Bool
      !expectedScripts = hasExpectedScripts txInfoInputs thisValidatorHash dynamicConfigDatum'voteValidator

      -- Check that Value contains the 'voteNft' token
      -- This acts like a pass token that allows the user to vote
      hasVotePassToken :: Value -> Maybe Value
      hasVotePassToken (Value v) =
        case filter (\(k, _) -> dynamicConfigDatum'voteNft == k) (M.toList v) of
          [] -> Nothing
          xs@[_] -> Just (Value (M.fromList xs))
          _ -> traceError "Too many vote nfts"

      -- Check for the presence of the vote token minted by
      -- the 'mkVoteMinter' policy when casting a vote on a proposal
      hasVoteWitness :: Value -> Bool
      hasVoteWitness = hasOneOfToken dynamicConfigDatum'voteCurrencySymbol dynamicConfigDatum'voteTokenName

      thisTallyTokenName :: TokenName
      thisTallyTokenName = getTokenNameOfNft dynamicConfigDatum'tallyNft oldValue "Tally Nft"

      -- Helper for loop that counts the votes
      stepVotes ::
        TxInInfo ->
        (Integer, Integer, Map Address Value) ->
        (Integer, Integer, Map Address Value)
      stepVotes
        TxInInfo {txInInfoResolved = TxOut {..}}
        oldAcc@(oldForCount, oldAgainstCount, oldPayoutMap) =
          case (hasVotePassToken txOutValue, hasVoteWitness txOutValue) of
            (Just voteNft, True) ->
              let
                VoteDatum {..} = convertDatum txInfoData txOutDatum

                -- Count all the dynamicConfigDatum'voteFungibleCurrencySymbol
                -- with dynamicConfigDatum'voteFungibleTokenName tokens on the vote utxo
                fungibleTokens :: Integer
                !fungibleTokens =
                  countOfTokenInValue
                    dynamicConfigDatum'voteFungibleCurrencySymbol
                    dynamicConfigDatum'voteFungibleTokenName
                    txOutValue

                -- Calculate fungible votes using the dynamicConfigDatum'fungibleVotePercent
                fungibleVotes :: Integer
                !fungibleVotes
                  | fungibleTokens == 0 = 0
                  | otherwise = (fungibleTokens * dynamicConfigDatum'fungibleVotePercent) `divide` 1000

                -- Add the lovelaces and the NFT
                votePayout :: Value
                !votePayout =
                  if fungibleTokens == 0
                    then Value (M.insert adaSymbol (M.singleton adaToken voteDatum'returnAda) (getValue voteNft))
                    else
                      Value $
                        M.insert
                          dynamicConfigDatum'voteFungibleCurrencySymbol
                          (M.singleton dynamicConfigDatum'voteFungibleTokenName fungibleTokens)
                          ( M.insert
                              adaSymbol
                              (M.singleton adaToken voteDatum'returnAda)
                              (getValue voteNft)
                          )

                checkProposal :: Bool
                !checkProposal = voteDatum'proposalTokenName == thisTallyTokenName

                newForCount :: Integer
                !newForCount = oldForCount + if voteDatum'direction == VoteDirection'For then 1 + fungibleVotes else 0

                newAgainstCount :: Integer
                !newAgainstCount =
                  oldAgainstCount
                    + if voteDatum'direction == VoteDirection'For then 0 else 1 + fungibleVotes

                newPayoutMap :: Map Address Value
                !newPayoutMap = mergePayouts voteDatum'voteOwner votePayout oldPayoutMap
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
      !tallyingIsInactive = tallyStateDatum'proposalEndTime `before` txInfoValidRange

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
            { tallyStateDatum'for = oldFor + forCount
            , tallyStateDatum'against = oldAgainst + againstCount
            }
     in
      traceIfFalse "Tally is active" tallyingIsInactive
        && traceIfFalse "Unexpected scripts" expectedScripts
        && traceIfFalse "Not all vote tokens and Ada returned" voteNftAndAdaToVoters
        && traceIfFalse "Not all vote tokens are burned" voteTokenAreAllBurned
        && traceIfFalse "Tally datum is not updated" tallyDatumIsUpdated
        && traceIfFalse "Old value is not as big as new value" newValueIsAtleastAsBigAsOldValue
validateTally _ _ _ _ = traceError "Wrong script purpose"

tallyValidatorCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
tallyValidatorCompiledCode = $$(PlutusTx.compile [||untypedTallyValidator||])

untypedTallyValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTallyValidator = untypedValidator validateTally
