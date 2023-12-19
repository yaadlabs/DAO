{- |
Module: Dao.Treasury.Script
Description: Dao treasury related scripts. It includes:
  - Treasury validator script
-}
module Dao.Treasury.Script (
  -- * Validator
  validateTreasury,
  treasuryValidatorCompiledCode,
  treasuryValidatorUnappliedCompiledCode,
) where

import Dao.ScriptArgument (
  ConfigurationValidatorConfig (
    ConfigurationValidatorConfig,
    cvcConfigNftCurrencySymbol,
    cvcConfigNftTokenName
  ),
 )
import Dao.Shared (
  convertDatum,
  hasOneOfToken,
  hasSymbolInValue,
  hasTokenInValue,
  isScriptCredential,
  lovelacesOf,
  mkUntypedValidator,
  wrapValidate,
 )
import LambdaBuffers.ApplicationTypes.Configuration (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dynamicConfigDatum'agentDisbursementPercent,
    dynamicConfigDatum'generalMajorityPercent,
    dynamicConfigDatum'generalRelativeMajorityPercent,
    dynamicConfigDatum'maxGeneralDisbursement,
    dynamicConfigDatum'maxTripDisbursement,
    dynamicConfigDatum'proposalTallyEndOffset,
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
  ScriptPurpose (Spending),
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
  applyCode,
  compile,
  liftCode,
 )
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Integer,
  Maybe (Just, Nothing),
  divide,
  filter,
  mapMaybe,
  mconcat,
  min,
  otherwise,
  traceError,
  traceIfFalse,
  (&&),
  (*),
  (+),
  (-),
  (.),
  (/=),
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

        - That exactly one 'upgradeMinter' token was minted. The CurrencySymbol for this token
          is provided as the field of the 'Upgrade' constructor of the Proposal type.
-}
validateTreasury ::
  ConfigurationValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateTreasury
  ConfigurationValidatorConfig {..}
  _treasury
  _action
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Spending thisTxRef
    } =
    let
      -- Check that there is only one of this script in the inputs
      (!inputValue, !thisValidator) :: (Value, ScriptHash) = ownValueAndValidator txInfoInputs thisTxRef

      -- Helper for filtering for config UTXO
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken cvcConfigNftCurrencySymbol cvcConfigNftTokenName

      -- Get the DynamicConfigDatum from the reference inputs, should be exactly one
      DynamicConfigDatum {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one config in the reference inputs"

      -- Helper for filtering for tally UTXO
      hasTallyNft :: Value -> Bool
      hasTallyNft = hasSymbolInValue dynamicConfigDatum'tallyNft

      -- Get the TallyStateDatum from the reference inputs, should be exactly one
      TallyStateDatum {..} =
        case filter (hasTallyNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [] -> traceError "Missing tally NFT"
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Too many tally NFT values"

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

              -- Make sure the disbursed amount is less than the max
              -- Find the total value returned to the script address
              outputValue :: Value
              !outputValue = case getContinuingOutputs' thisValidator txInfoOutputs of
                [TxOut {..}] -> txOutValue
                _ -> traceError "Should be exactly one continuing treasury output"

              outputValueIsLargeEnough :: Bool
              !outputValueIsLargeEnough = outputValue `geq` (inputValue - disbursedAmount)

              -- Paid the ptGeneralPaymentAddress the ptGeneralPaymentValue
              paidToTravelAgentAddress :: Bool
              !paidToTravelAgentAddress =
                lovelacesOf (valuePaidTo' txInfoOutputs travelAgentAddress) >= travelAgentLovelaces

              paidToTravelerAddress :: Bool
              !paidToTravelerAddress =
                lovelacesOf (valuePaidTo' txInfoOutputs travelerAddress) >= travelerLovelaces
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && traceIfFalse "Disbursing too much" outputValueIsLargeEnough
                && traceIfFalse "Not paying enough to the travel agent address" paidToTravelAgentAddress
                && traceIfFalse "Not paying enough to the traveler address" paidToTravelerAddress
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

              -- Make sure the disbursed amount is less than the max
              -- Find the total value returned to the script address
              outputValue :: Value
              !outputValue = case getContinuingOutputs' thisValidator txInfoOutputs of
                [TxOut {..}] -> txOutValue
                _ -> traceError "expected exactly one continuing output"

              outputValueIsLargeEnough :: Bool
              !outputValueIsLargeEnough = outputValue `geq` (inputValue - disbursedAmount)

              -- Paid the ptGeneralPaymentAddress the ptGeneralPaymentValue
              paidToAddress :: Bool
              !paidToAddress =
                lovelacesOf (valuePaidTo' txInfoOutputs generalPaymentAddress) >= generalPaymentValue
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && traceIfFalse "Disbursing too much" outputValueIsLargeEnough
                && traceIfFalse "Not paying to the correct address" paidToAddress
          ProposalType'Upgrade upgradeMinter ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse
                  "relative majority is too low"
                  (relativeMajority >= dynamicConfigDatum'upgradeRelativeMajorityPercent)
                  && traceIfFalse
                    "majority is too small"
                    (majorityPercent >= dynamicConfigDatum'upgradeMajorityPercent)

              -- Make sure the upgrade token was minted
              hasUpgradeMinterToken :: Bool
              !hasUpgradeMinterToken = hasTokenInValue upgradeMinter "Treasury Minter" txInfoMint
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && traceIfFalse "Not minting upgrade token" hasUpgradeMinterToken
                && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime
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
  ScriptHash ->
  [TxOut] ->
  [TxOut]
getContinuingOutputs' vh =
  filter
    ( \TxOut {..} ->
        addressCredential txOutAddress
          == ScriptCredential vh
    )

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

treasuryValidatorUnappliedCompiledCode ::
  CompiledCode (ConfigurationValidatorConfig -> BuiltinData -> BuiltinData -> BuiltinData -> ())
treasuryValidatorUnappliedCompiledCode =
  $$(PlutusTx.compile [||mkUntypedValidator . validateTreasury||])

treasuryValidatorCompiledCode ::
  ConfigurationValidatorConfig ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
treasuryValidatorCompiledCode config =
  $$(PlutusTx.compile [||wrapValidate validateTreasury||]) `applyCode` liftCode config
