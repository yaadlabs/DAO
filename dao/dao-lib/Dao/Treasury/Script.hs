{- |
Module: Dao.Treasury.Script
Description: Dao treasury related scripts. It includes:
  - Treasury validator script
-}
module Dao.Treasury.Script (
  -- * Treasury validator
  validateTreasury,
  treasuryValidatorCompiledCode,

  -- * Treasury policy (Placeholder for off-chain use)
  treasuryPolicyCompiledCode,
) where

import Dao.ScriptArgument (
  ValidatorParams (
    ValidatorParams,
    vpConfigSymbol,
    vpConfigTokenName
  ),
 )
import Dao.Shared (
  convertDatum,
  hasBurnedTokens,
  hasExactAssetCount,
  hasOneOfToken,
  hasSingleTokenWithSymbolAndTokenName,
  hasSymbolInValue,
  isScriptCredential,
  lovelacesOf,
  untypedPolicy,
  untypedValidator,
 )
import LambdaBuffers.ApplicationTypes.Configuration (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dynamicConfigDatum'agentDisbursementPercent,
    dynamicConfigDatum'generalMajorityPercent,
    dynamicConfigDatum'generalRelativeMajorityPercent,
    dynamicConfigDatum'maxGeneralDisbursement,
    dynamicConfigDatum'maxTripDisbursement,
    dynamicConfigDatum'minTreasuryValue,
    dynamicConfigDatum'proposalTallyEndOffset,
    dynamicConfigDatum'protocolFallbackAddress,
    dynamicConfigDatum'tallyNft,
    dynamicConfigDatum'totalVotes,
    dynamicConfigDatum'tripMajorityPercent,
    dynamicConfigDatum'tripRelativeMajorityPercent,
    dynamicConfigDatum'upgradeMajorityPercent,
    dynamicConfigDatum'upgradeRelativeMajorityPercent
  ),
 )
import LambdaBuffers.ApplicationTypes.Proposal (
  ProposalType (
    ProposalType'General,
    ProposalType'Trip,
    ProposalType'Upgrade
  ),
 )
import LambdaBuffers.ApplicationTypes.Tally (
  TallyStateDatum (
    TallyStateDatum,
    tallyStateDatum'against,
    tallyStateDatum'for,
    tallyStateDatum'proposal,
    tallyStateDatum'proposalEndTime
  ),
 )
import LambdaBuffers.ApplicationTypes.Treasury (
  TreasuryDatum (TreasuryDatum),
 )
import PlutusLedgerApi.V1.Address (Address (Address, addressCredential))
import PlutusLedgerApi.V1.Credential (Credential (ScriptCredential))
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime))
import PlutusLedgerApi.V1.Value (
  Value,
  adaSymbol,
  adaToken,
  geq,
  singleton,
 )
import PlutusLedgerApi.V2.Contexts (
  ScriptContext (
    ScriptContext,
    scriptContextPurpose,
    scriptContextTxInfo
  ),
  ScriptPurpose (Minting, Spending),
  TxInInfo (
    TxInInfo,
    txInInfoOutRef,
    txInInfoResolved
  ),
  TxInfo (
    TxInfo,
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoReferenceInputs,
    txInfoValidRange
  ),
  TxOut (
    TxOut,
    txOutAddress,
    txOutDatum,
    txOutValue
  ),
  TxOutRef,
 )
import PlutusTx (
  CompiledCode,
  compile,
 )
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Integer,
  Maybe (Just, Nothing),
  any,
  divide,
  filter,
  mapMaybe,
  mconcat,
  min,
  not,
  otherwise,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (*),
  (+),
  (-),
  (.),
  (/=),
  (<),
  (==),
  (>=),
 )

{- | Validator for treasury.

   == Common checks

     The validator always ensures:

      - There is exactly one of this script contained in the transaction's inputs.
        This check is carried out using the 'Dao.Treasury.Script.ownValueAndValidator' helper.

      - It uses the 'tallyStateDatum'proposal' field of 'Dao.Types.TallyStateDatum' like a redeemer,
        choosing which branch to follow based on the value of this field. (Trip, General, or Upgrade)

   == Trip proposal

      When the 'tallyStateDatum'proposal' field of 'LambdaBuffers.ApplicationTypes.Tally.TallyStateDatum'
      is set to 'Trip', this validator performs the following checks:

        - The proposal has enough votes. The vote counts equal or exceed the values specified in
          the 'tripRelativeMajorityPercent' and 'tripMajorityPercent' fields of the
          'LambdaBuffers.ApplicationTypes.Configuration.DynamicConfigDatum'.

        - The amount disbursed does not exceed the amount specified in the 'maxTripDisbursement'
          field of the 'DynamicConfigDatum'.

        - The correct amount is paid to the traveler's address, specified by the
          corresponding 'Trip' field in the 'ProposalType'. The traveler's amount should
          be greater than or equal to the total cost of the travel minus the payment to
          the travel agent.

        - The correct amount is paid to the travel agent's address, specified by the
          corresponding 'Trip' field in the 'ProposalType'.

   == General proposal

      When the 'tallyStateDatum'proposal' field of 'TallyStateDatum'
      is set to 'General', this validator performs the following checks:

        - The proposal has enough votes. The vote counts equal or exceed the values specified in
          the 'generalRelativeMajorityPercent' and 'generalMajorityPercent' fields of the
          'DynamicConfigDatum'.

        - The amount disbursed does not exceed the amount specified in the 'maxGeneralDisbursement'
          field of the 'DynamicConfigDatum'.

        - The correct amount is paid to the general payment address, specified by the
          corresponding 'General' field in the 'ProposalType'.

   == Upgrade proposal

      When the 'tallyStateDatum'proposal' field of 'TallyStateDatum'
      is set to 'Upgrade', this validator performs the following checks:

        - The proposal has enough votes. The vote counts equal or exceed the values specified in
          the 'upgradeRelativeMajorityPercent' and 'upgradeMajorityPercent' fields of the
          'DynamicConfigDatum'.

        - That the proposal end time has passed. We do this by checking that the sum of
          the 'tallyStateDatum'proposalEndtime' field of the 'TallyStateDatum' and
          the 'proposalTallyEndOffset' of the 'DynamicConfigDatum' against
          the validity range of the transaction. Ensuring the sum of these values is less than the range.

        - That the tally NFT is burned to prevent double-funding or reuse of the proposal.
          (Previously ensured by minting a separate upgrade token.)
-}
validateTreasury ::
  ValidatorParams ->
  TreasuryDatum ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateTreasury
  ValidatorParams {..}
  _treasuryDatum
  _action
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Spending thisTxRef
    } =
    let
      -- Check that there is only one of this script in the inputs
      (!inputValue, !thisValidator) :: (Value, ScriptHash) = ownValueAndValidator txInfoInputs thisTxRef

      -- Get the full address of this treasury validator (including staking credential)
      thisValidatorAddress :: Address
      thisValidatorAddress = case filter (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef == thisTxRef) txInfoInputs of
        [TxInInfo {txInInfoResolved = TxOut {txOutAddress}}] -> txOutAddress
        _ -> traceError "Treasury input"

      validateRouting :: Value -> Bool
      validateRouting remainingValue =
        let
          remainingLovelaces :: Integer
          !remainingLovelaces = lovelacesOf remainingValue

          isBelowThreshold :: Bool
          !isBelowThreshold = remainingLovelaces < dynamicConfigDatum'minTreasuryValue
         in
          if isBelowThreshold
            then
              let
                noContinuingOutput :: Bool
                !noContinuingOutput = case getContinuingOutputs' thisValidatorAddress txInfoOutputs of
                  [] -> True
                  _ -> False

                fallbackValue :: Value
                !fallbackValue = valuePaidTo' txInfoOutputs dynamicConfigDatum'protocolFallbackAddress

                sentToFallback :: Bool
                !sentToFallback = fallbackValue `geq` remainingValue
               in
                traceIfFalse "Below threshold" noContinuingOutput
                  && traceIfFalse "Fallback" sentToFallback
            else case getContinuingOutputs' thisValidatorAddress txInfoOutputs of
              [TxOut {txOutValue = val, txOutDatum = datum}] ->
                let
                  continuingDatum :: TreasuryDatum = convertDatum txInfoData datum

                  validDatum :: Bool
                  !validDatum = continuingDatum == TreasuryDatum True

                  valueIsCorrect :: Bool
                  !valueIsCorrect = val `geq` remainingValue

                  noDustTokens :: Bool
                  !noDustTokens = hasExactAssetCount val 1
                 in
                  traceIfFalse "Invalid continuing treasury datum" validDatum
                    && traceIfFalse "Disbursing too much" valueIsCorrect
                    && traceIfFalse "Continuing output contains dust tokens" noDustTokens
              _ -> traceError "Should be exactly one continuing treasury output"

      -- Helper for filtering for config UTXO
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken vpConfigSymbol vpConfigTokenName

      -- Get the DynamicConfigDatum from the reference inputs, should be exactly one
      DynamicConfigDatum {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Config ref"

      -- Helper for filtering for tally UTXO
      hasTallyNft :: Value -> Bool
      hasTallyNft = hasSymbolInValue dynamicConfigDatum'tallyNft

      -- Get the TallyStateDatum from the spent inputs (not reference), should be exactly one
      -- This prevents the same proposal from being funded multiple times
      TallyStateDatum {..} =
        case filter (hasTallyNft . txOutValue . txInInfoResolved) txInfoInputs of
          [] -> traceError "Tally missing"
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Tally count"

      -- Verify the Tally NFT is being burned
      -- This ensures the proposal cannot be funded again
      tallyNftIsBurned :: Bool
      !tallyNftIsBurned = hasBurnedTokens dynamicConfigDatum'tallyNft txInfoMint "Tally NFT burn"

      -- Calculate the values needed for the corresponding checks
      totalVotes :: Integer
      !totalVotes = tallyStateDatum'for + tallyStateDatum'against

      relativeMajority :: Integer
      !relativeMajority = (totalVotes * 1000) `divide` dynamicConfigDatum'totalVotes

      majorityPercent :: Integer
      !majorityPercent = (tallyStateDatum'for * 1000) `divide` totalVotes

      isAfterTallyEndTime :: Bool
      !isAfterTallyEndTime =
        (tallyStateDatum'proposalEndTime + POSIXTime dynamicConfigDatum'proposalTallyEndOffset) `before` txInfoValidRange
     in
      onlyOneOfThisScript txInfoInputs thisValidator thisTxRef
        && case tallyStateDatum'proposal of
          ProposalType'Trip travelAgentAddress travelerAddress totalTravelCost ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse
                  "relative majority is too low"
                  (relativeMajority >= dynamicConfigDatum'tripRelativeMajorityPercent)
                  && traceIfFalse
                    "majority is too small"
                    (majorityPercent >= dynamicConfigDatum'tripMajorityPercent)

              -- Get the disbursed amount
              disbursedAmount :: Value
              !disbursedAmount = singleton adaSymbol adaToken (min dynamicConfigDatum'maxTripDisbursement totalTravelCost)

              travelAgentLovelaces :: Integer
              !travelAgentLovelaces = (totalTravelCost * dynamicConfigDatum'agentDisbursementPercent) `divide` 1000

              travelerLovelaces :: Integer
              !travelerLovelaces = totalTravelCost - travelAgentLovelaces

              remainingValue :: Value
              !remainingValue = inputValue - disbursedAmount

              routingIsValid :: Bool
              !routingIsValid = validateRouting remainingValue

              -- Paid the ptGeneralPaymentAddress the ptGeneralPaymentValue
              paidToTravelAgentAddress :: Bool
              !paidToTravelAgentAddress =
                lovelacesOf (valuePaidTo' txInfoOutputs travelAgentAddress) >= travelAgentLovelaces

              paidToTravelerAddress :: Bool
              !paidToTravelerAddress =
                lovelacesOf (valuePaidTo' txInfoOutputs travelerAddress) >= travelerLovelaces

              -- Ensure payment addresses are not script addresses
              travelAgentIsNotScript :: Bool
              !travelAgentIsNotScript =
                not $ isScriptCredential (addressCredential travelAgentAddress)

              travelerIsNotScript :: Bool
              !travelerIsNotScript =
                not $ isScriptCredential (addressCredential travelerAddress)
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && routingIsValid
                && traceIfFalse "Not paying enough to the travel agent address" paidToTravelAgentAddress
                && traceIfFalse "Not paying enough to the traveler address" paidToTravelerAddress
                && traceIfFalse "Travel agent address cannot be script address" travelAgentIsNotScript
                && traceIfFalse "Traveler address cannot be script address" travelerIsNotScript
                && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime
                && tallyNftIsBurned
          ProposalType'General generalPaymentAddress generalPaymentValue ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse
                  "relative majority is too low"
                  (relativeMajority >= dynamicConfigDatum'generalRelativeMajorityPercent)
                  && traceIfFalse
                    "majority is too small"
                    (majorityPercent >= dynamicConfigDatum'generalMajorityPercent)

              -- Get the disbursed amount
              disbursedAmount :: Value
              !disbursedAmount =
                singleton
                  adaSymbol
                  adaToken
                  (min dynamicConfigDatum'maxGeneralDisbursement generalPaymentValue)

              remainingValue :: Value
              !remainingValue = inputValue - disbursedAmount

              routingIsValid :: Bool
              !routingIsValid = validateRouting remainingValue

              -- Paid the ptGeneralPaymentAddress the ptGeneralPaymentValue
              paidToAddress :: Bool
              !paidToAddress =
                lovelacesOf (valuePaidTo' txInfoOutputs generalPaymentAddress) >= generalPaymentValue

              -- Ensure payment address is not a script address
              generalPaymentIsNotScript :: Bool
              !generalPaymentIsNotScript =
                not $ isScriptCredential (addressCredential generalPaymentAddress)
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && routingIsValid
                && traceIfFalse "Not paying to the correct address" paidToAddress
                && traceIfFalse "General payment address cannot be script address" generalPaymentIsNotScript
                && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime
                && tallyNftIsBurned
          ProposalType'Upgrade maybeNewTreasuryAddress ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse
                  "relative majority is too low"
                  (relativeMajority >= dynamicConfigDatum'upgradeRelativeMajorityPercent)
                  && traceIfFalse
                    "majority is too small"
                    (majorityPercent >= dynamicConfigDatum'upgradeMajorityPercent)

              -- Validate treasury migration based on Maybe Address
              treasuryMigrationValid :: Bool
              !treasuryMigrationValid = case maybeNewTreasuryAddress of
                Nothing ->
                  -- Config-only upgrade: Treasury should NOT be spent
                  -- This validator only runs if Treasury is being spent, so this should fail
                  traceError "Upgrade does not allow Treasury spending (config-only upgrade)"
                Just newTreasuryAddress ->
                  -- Treasury migration: Validate all funds go to new address
                  let
                    -- Ensure no continuing output to old Treasury
                    noContinuingOutput :: Bool
                    !noContinuingOutput = case getContinuingOutputs' thisValidatorAddress txInfoOutputs of
                      [] -> True
                      _ -> False

                    fundsToNewTreasury :: Value
                    !fundsToNewTreasury = valuePaidTo' txInfoOutputs newTreasuryAddress

                    -- Verify all funds sent to new treasury address
                    fundsSentToNewTreasury :: Bool
                    !fundsSentToNewTreasury = fundsToNewTreasury `geq` inputValue
                   in
                    traceIfFalse "Treasury migration: continuing output to old treasury detected" noContinuingOutput
                      && traceIfFalse "Treasury migration: funds not fully transferred to new address" fundsSentToNewTreasury
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && treasuryMigrationValid
                && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime
                && tallyNftIsBurned
validateTreasury _ _ _ _ = traceError "Wrong script purpose"

addressOutputsAt :: Address -> [TxOut] -> [Value]
addressOutputsAt addr outs =
  let
    flt TxOut {txOutAddress, txOutValue}
      | addr == txOutAddress = Just txOutValue
      | otherwise = Nothing
   in
    mapMaybe flt outs

valuePaidTo' :: [TxOut] -> Address -> Value
valuePaidTo' outs addr = mconcat (addressOutputsAt addr outs)

getContinuingOutputs' ::
  Address ->
  [TxOut] ->
  [TxOut]
getContinuingOutputs' expectedAddr =
  filter (\TxOut {txOutAddress} -> txOutAddress == expectedAddr)

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

onlyOneOfThisScript :: [TxInInfo] -> ScriptHash -> TxOutRef -> Bool
onlyOneOfThisScript ins vh expectedRef = go ins
  where
    go = \case
      [] -> True
      TxInInfo {txInInfoOutRef, txInInfoResolved = TxOut {txOutAddress = Address {..}}} : xs ->
        if isScriptCredential addressCredential && txInInfoOutRef /= expectedRef
          then case addressCredential of
            ScriptCredential vh' | vh' == vh -> False
            _ -> go xs
          else go xs

treasuryValidatorCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
treasuryValidatorCompiledCode = $$(PlutusTx.compile [||untypedTreasuryValidator||])

untypedTreasuryValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTreasuryValidator = untypedValidator validateTreasury

{- A one-shot minting policy. (Placeholder)

  Used in the off-chain when creating a new UTXO at the treasury validator
  and sending funds to that UTXO.
-}
mkTreasuryMinter :: TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkTreasuryMinter
  txOutRef
  _
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Minting thisCurrencySymbol
    } =
    let
      -- Ensure that the reference UTXO is spent
      hasUTxO :: Bool
      !hasUTxO = any (\i -> txInInfoOutRef i == txOutRef) txInfoInputs

      -- Ensure exactly one valid index token is minted
      onlyOneTokenMinted :: Bool
      !onlyOneTokenMinted =
        hasSingleTokenWithSymbolAndTokenName
          txInfoMint
          thisCurrencySymbol
          adaToken
     in
      traceIfFalse "Reference UTXO should be spent" hasUTxO
        && traceIfFalse "Exactly one valid token should be minted" onlyOneTokenMinted
mkTreasuryMinter _ _ _ = traceError "Wrong type of script purpose!"

untypedTreasuryPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedTreasuryPolicy = untypedPolicy mkTreasuryMinter

treasuryPolicyCompiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
treasuryPolicyCompiledCode = $$(PlutusTx.compile [||untypedTreasuryPolicy||])
