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
  hasExactAssetCount,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  integerToByteString,
  isScriptCredential,
  untypedPolicy,
  untypedValidator,
 )
import LambdaBuffers.ApplicationTypes.Configuration (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dynamicConfigDatum'configurationValidator,
    dynamicConfigDatum'fungibleVotePercent,
    dynamicConfigDatum'tallyNft,
    dynamicConfigDatum'tallyValidator,
    dynamicConfigDatum'treasuryValidator,
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
import PlutusLedgerApi.V1.Scripts (ScriptHash (ScriptHash))
import PlutusLedgerApi.V1.Value (
  CurrencySymbol,
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
  Bool (False, True),
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
  null,
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

    When minting (creating new proposal):
    - (ID-303) The transaction must include a vote NFT in inputs (members-only proposal creation)
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
    - The tally output is at the tally validator with no staking credential
      (Corresponding to the tally script provided by the 'dynamicConfigDatum'tallyValidator'
       field of the 'Dao.Types.DynamicConfigDatum')

    When burning (funding proposals):
    - The Tally NFT exists in the transaction inputs
    - Additional validation performed by Treasury or Configuration validators
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
      -- Check if we're minting (positive) or burning (negative)
      -- Burns are used when funding proposals or during upgrades
      isBurning :: Bool
      isBurning = case M.lookup thisCurrencySymbol (getValue txInfoMint) of
        Nothing -> traceError "This currency symbol not found in mints"
        Just tokenMap -> case M.toList tokenMap of
          [(_, amount)] -> amount PlutusTx.< 0
          _ -> traceError "Expected exactly one token type"

      -- Helper for filtering for config UTXO in the reference inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken tpConfigSymbol tpConfigTokenName

      -- Get the configuration from the reference inputs
      DynamicConfigDatum {dynamicConfigDatum'tallyValidator, dynamicConfigDatum'voteNft} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one valid config in the reference inputs"
     in
      if isBurning
        then -- Burning logic: validate this is a legitimate burn (during funding or upgrade)
        -- The Treasury or Configuration validator will perform the actual checks
        -- We just need to verify the Tally NFT being burned exists in inputs

          let
            hasTallyInInputs :: Bool
            hasTallyInInputs =
              any (hasSymbolInValue thisCurrencySymbol . txOutValue . txInInfoResolved) txInfoInputs
           in
            traceIfFalse "Tally NFT being burned must exist in inputs" hasTallyInInputs
        else -- Minting logic (original): create new proposal

          let
            -- ID-303: Require vote pass token to prevent spam proposals
            -- Only DAO members (those with vote NFT) can create proposals
            hasVotePass :: Bool
            !hasVotePass = any (hasSymbolInValue dynamicConfigDatum'voteNft . txOutValue . txInInfoResolved) txInfoInputs

            -- Helper for filtering for index UTXO in the inputs
            hasIndexNft :: Value -> Bool
            hasIndexNft = hasOneOfToken tpIndexSymbol tpIndexTokenName

            -- Get the index datum from the inputs
            IndexDatum {indexDatum'index} = case filter (hasIndexNft . txOutValue . txInInfoResolved) txInfoInputs of
              [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
              [] -> traceError "No index NFT found in inputs"
              _ -> traceError "Should be exactly one valid Index NFT output"

            -- The token name must be set to the index value
            theTokenName :: TokenName
            !theTokenName = TokenName $ integerToByteString indexDatum'index

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
            tallyStateDatum :: TallyStateDatum
            tallyStateDatum = convertDatum txInfoData outputDatum

            -- The initial votes for and against must both be set to zero
            tallyIsInitializeToZero :: Bool
            !tallyIsInitializeToZero = tallyStateDatum'for tallyStateDatum == 0 && tallyStateDatum'against tallyStateDatum == 0

            -- The NFT must be at the full address of the tally validator
            -- We check payment credential matches the expected script hash
            outputOnTallyValidator :: Bool
            !outputOnTallyValidator = addressCredential outputAddress == ScriptCredential dynamicConfigDatum'tallyValidator

            -- Additionally verify no staking credential redirection (prevent staking rewards theft)
            -- The output should have the same staking credential structure as any existing tally UTxO
            -- For new proposals, we require the address to have no staking credential
            -- (To prevent attackers from adding staking credentials)
            noStakingCredential :: Bool
            !noStakingCredential = case outputAddress of
              Address _ Nothing -> True
              _ -> False

            -- Ensure exactly one valid tally token is minted
            onlyOneTokenMinted :: Bool
            !onlyOneTokenMinted =
              hasSingleTokenWithSymbolAndTokenName
                txInfoMint
                thisCurrencySymbol
                theTokenName
           in
            traceIfFalse "Proposal creator must have vote pass (member-only)" hasVotePass
              && traceIfFalse "Tally datum vote counts are not initialized to zero" tallyIsInitializeToZero
              && traceIfFalse "Tally NFT must be sent to the Tally validator" outputOnTallyValidator
              && traceIfFalse "Tally output must have no staking credential" noStakingCredential
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
      (!oldValue, !thisValidatorHash) :: (Value, ScriptHash) = ownValueAndValidator txInfoInputs thisOutRef

      -- ID-501: Check if ANY tally NFT is being burned (by checking oldValue symbol)
      -- We check oldValue symbol instead of config symbol to avoid needing config first
      thisTallySymbol :: CurrencySymbol
      thisTallySymbol = case [(cs, tn) | (cs, m) <- M.toList (getValue oldValue), cs PlutusTx./= adaSymbol, (tn, amt) <- M.toList m, amt PlutusTx.> 0] of
        [(cs, _)] -> cs
        _ -> traceError "Could not determine tally symbol from oldValue"

      isTallyBeingBurned :: Bool
      isTallyBeingBurned = case M.lookup thisTallySymbol (getValue txInfoMint) of
        Nothing -> False
        Just tokenMap -> case M.toList tokenMap of
          [(_, amount)] -> amount PlutusTx.< 0
          _ -> False

      emptyScriptHash :: ScriptHash
      emptyScriptHash = ScriptHash ""

      -- Helper for filtering for config UTXO in the reference inputs or inputs
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken vpConfigSymbol vpConfigTokenName

      configSources :: [TxInInfo]
      configSources =
        let
          refMatches = filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs
         in
          if null refMatches
            then filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoInputs
            else refMatches

      -- Get the 'DynamicConfig' from either the reference inputs (preferred) or from spent inputs
      DynamicConfigDatum {..} = case configSources of
        [] -> traceError "Missing configuration datum"
        [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
        _ -> traceError "Too many configuration datums"

      thisValidatorAddress :: Address
      thisValidatorAddress = case filter (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef == thisOutRef) txInfoInputs of
        [TxInInfo {txInInfoResolved = TxOut {txOutAddress}}] -> txOutAddress
        _ -> traceError "Could not find this tally input"

      scriptCredentials :: [Credential]
      scriptCredentials =
        filter
          isScriptCredential
          (map (addressCredential . txOutAddress . txInInfoResolved) txInfoInputs)

      tallyCredential :: Credential
      tallyCredential = ScriptCredential thisValidatorHash

      credentialListFromHash :: ScriptHash -> [Credential]
      credentialListFromHash hash
        | hash == emptyScriptHash = mempty
        | otherwise = [ScriptCredential hash]

      allowedBurnCredentials :: [Credential]
      allowedBurnCredentials =
        tallyCredential
          : foldr
            (\hash acc -> credentialListFromHash hash <> acc)
            []
            [ dynamicConfigDatum'voteValidator
            , dynamicConfigDatum'treasuryValidator
            , dynamicConfigDatum'configurationValidator
            ]

      credentialInList :: Credential -> [Credential] -> Bool
      credentialInList cred = foldr (\candidate acc -> acc || cred == candidate) False

      hasConfigInput :: Bool
      hasConfigInput = any (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoInputs

      treasuryCredential :: [Credential]
      treasuryCredential = credentialListFromHash dynamicConfigDatum'treasuryValidator

      hasTreasuryScriptInput :: Bool
      hasTreasuryScriptInput = any (`credentialInList` treasuryCredential) scriptCredentials

      hasCompanionInput :: Bool
      hasCompanionInput = hasConfigInput || hasTreasuryScriptInput

      hasOnlyAllowedInputs :: Bool
      hasOnlyAllowedInputs = all (\cred -> credentialInList cred allowedBurnCredentials) scriptCredentials

      hasSingleTallyInput :: Bool
      hasSingleTallyInput = length (filter (== tallyCredential) scriptCredentials) == 1
     in
      if isTallyBeingBurned
        then
          traceIfFalse "Tally burn must include treasury or configuration input" hasCompanionInput
            && traceIfFalse "Unexpected script credentials while burning tally" hasOnlyAllowedInputs
            && traceIfFalse "More than one tally input" hasSingleTallyInput
        else -- Normal vote counting flow: validate continuing output and vote counting

          let
            -- Make sure there is only one tally and many votes (only for vote counting)
            expectedScripts :: Bool
            !expectedScripts = hasExpectedScripts txInfoInputs thisValidatorHash dynamicConfigDatum'voteValidator

            tallyingIsInactive :: Bool
            !tallyingIsInactive = tallyStateDatum'proposalEndTime `before` txInfoValidRange

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

            voteTokenAreAllBurned :: Bool
            !voteTokenAreAllBurned = not $ any (hasVoteWitness . txOutValue) txInfoOutputs

            (!newValue, !newDatum) :: (Value, TallyStateDatum) =
              case filter (\TxOut {txOutAddress} -> txOutAddress == thisValidatorAddress) txInfoOutputs of
                [TxOut {..}] -> (txOutValue, convertDatum txInfoData txOutDatum)
                _ -> traceError "Wrong number of continuing outputs"

            -- Ensure the tally NFT remains at the validator
            newValueIsAtleastAsBigAsOldValue :: Bool
            !newValueIsAtleastAsBigAsOldValue = newValue `geq` oldValue

            -- Ensure exactly 2 assets (ADA + Tally NFT) to prevent dust attacks
            noDustTokens :: Bool
            !noDustTokens = hasExactAssetCount newValue 2

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
              && traceIfFalse "Continuing output contains dust tokens" noDustTokens
validateTally _ _ _ _ = traceError "Wrong script purpose"

tallyValidatorCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
tallyValidatorCompiledCode = $$(PlutusTx.compile [||untypedTallyValidator||])

untypedTallyValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTallyValidator = untypedValidator validateTally
