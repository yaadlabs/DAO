{- |
Module: Triphut.Treasury.Script
Description: Triphut treasury related scripts. It includes:
  - Treasury validator script
-}
module Triphut.Treasury.Script (
  -- * Validator
  treasuryScript,
  treasuryValidator,
  treasuryValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript, PlutusScriptV2)
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Scripts (Validator, ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Value (
  Value,
  adaSymbol,
  adaToken,
  geq,
  singleton,
 )
import Plutus.V2.Ledger.Contexts (
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
 )
import Plutus.V2.Ledger.Tx hiding (Mint)
import PlutusTx (
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
import Triphut.Shared (
  convertDatum,
  hasOneOfToken,
  hasSymbolInValue,
  hasTokenInValue,
  isScriptCredential,
  lovelacesOf,
  mkValidatorWithSettings,
  validatorHash,
  validatorToScript,
  wrapValidate,
 )
import Triphut.Treasury (
  TreasuryValidatorConfig (
    TreasuryValidatorConfig,
    tvcConfigNftCurrencySymbol,
    tvcConfigNftTokenName
  ),
 )
import Triphut.Types (
  DynamicConfigDatum (
    DynamicConfigDatum,
    dcAgentDisbursementPercent,
    dcGeneralMajorityPercent,
    dcGeneralRelativeMajorityPercent,
    dcMaxGeneralDisbursement,
    dcMaxTripDisbursement,
    dcProposalTallyEndOffset,
    dcTallyNft,
    dcTotalVotes,
    dcTripMajorityPercent,
    dcTripRelativeMajorityPercent,
    dcUpgradeMajorityPercent,
    dcUpgradeRelativeMajorityPercent
  ),
  ProposalType (General, Trip, Upgrade),
  TallyStateDatum (TallyStateDatum, tsAgainst, tsFor, tsProposal, tsProposalEndTime),
 )

{- | Validator for treasury.

   == Common checks

     The validator always ensures:

      - There is exactly one of this script contained in the transaction's inputs.
        This check is carried out using the 'Triphut.Treasury.Script.ownValueAndValidator' helper.

      - It uses the 'tsProposal' field of 'Triphut.Types.TallyStateDatum' like a redeemer,
        choosing which branch to follow based on the value of this field. (Trip, General, or Upgrade)

   == Trip proposal

      When the 'tsProposal' field of 'Triphut.Types.TallyStateDatum'
      is set to 'Trip', this validator performs the following checks:

        - The proposal has enough votes. The vote counts equal or exceed the values specified in
          the 'dcTripRelativeMajorityPercent' and 'dcTripMajorityPercent' fields of the
          'Triphut.Types.DynamicConfigDatum'.

        - The amount disbursed does not exceed the amount specified in the 'dcMaxTripDisbursement'
          field of the 'Triphut.Types.DynamicConfigDatum'.

        - The correct amount is paid to the traveler's address, specified by the
          corresponding 'Trip' field in the 'ProposalType'. The traveler's amount should
          be greater than or equal to the total cose of the travel minus the payment to
          the travel agent.

        - The correct amount is paid to the travel agent's address, specified by the
          corresponding 'Trip' field in the 'ProposalType'.

   == General proposal

      When the 'tsProposal' field of 'Triphut.Types.TallyStateDatum'
      is set to 'General', this validator performs the following checks:

        - The proposal has enough votes. The vote counts equal or exceed the values specified in
          the 'dcGeneralRelativeMajorityPercent' and 'dcGeneralMajorityPercent' fields of the
          'Triphut.Types.DynamicConfigDatum'.

        - The amount disbursed does not exceed the amount specified in the 'dcMaxGeneralDisbursement'
          field of the 'Triphut.Types.DynamicConfigDatum'.

        - The correct amount is paid to the general payment address, specified by the
          corresponding 'General' field in the 'ProposalType'.

   == Upgrade proposal

      When the 'tsProposal' field of 'Triphut.Types.TallyStateDatum'
      is set to 'Upgrade', this validator performs the following checks:

        - The proposal has enough votes. The vote counts equal or exceed the values specified in
          the 'dcUpgradeRelativeMajorityPercent' and 'dcUpgradeMajorityPercent' fields of the
          'Triphut.Types.DynamicConfigDatum'.

        - That the proposal end time has passed. We do this by checking that the sum of the 'tsProposalEndtime'
          field of the 'Triphut.Types.TallyStateDatum' and the 'dcProposalTallyEndOffset' of the
          'Triphut.Types.DynamicConfigDatum' against the validity range of the transaction.
          Ensuring the sum of these values is less than the range.

        - That exactly one 'upgradeMinter' token was minted. The CurrencySymbol for this token
          is provided as the field of the 'Upgrade' constructor of the Proposal type.
-}
validateTreasury ::
  TreasuryValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  ScriptContext ->
  Bool
validateTreasury
  TreasuryValidatorConfig {..}
  _treasury
  _action
  ScriptContext
    { scriptContextTxInfo = TxInfo {..}
    , scriptContextPurpose = Spending thisTxRef
    } =
    let
      -- Check that there is only one of this script in the inputs
      (!inputValue, !thisValidator) :: (Value, ValidatorHash) = ownValueAndValidator txInfoInputs thisTxRef

      -- Helper for filtering for config UTXO
      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken tvcConfigNftCurrencySymbol tvcConfigNftTokenName

      -- Get the DynamicConfigDatum from the reference inputs, should be exactly one
      DynamicConfigDatum {..} =
        case filter (hasConfigurationNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Should be exactly one config in the reference inputs"

      -- Helper for filtering for tally UTXO
      hasTallyNft :: Value -> Bool
      hasTallyNft = hasSymbolInValue dcTallyNft

      -- Get the TallyStateDatum from the reference inputs, should be exactly one
      TallyStateDatum {..} =
        case filter (hasTallyNft . txOutValue . txInInfoResolved) txInfoReferenceInputs of
          [] -> traceError "Missing tally NFT"
          [TxInInfo {txInInfoResolved = TxOut {..}}] -> convertDatum txInfoData txOutDatum
          _ -> traceError "Too many tally NFT values"

      -- Calculate the values needed for the corresponding checks
      totalVotes :: Integer
      !totalVotes = tsFor + tsAgainst

      relativeMajority :: Integer
      !relativeMajority = (totalVotes * 1000) `divide` dcTotalVotes

      majorityPercent :: Integer
      !majorityPercent = (tsFor * 1000) `divide` totalVotes

      isAfterTallyEndTime :: Bool
      !isAfterTallyEndTime =
        (tsProposalEndTime + POSIXTime dcProposalTallyEndOffset) `before` txInfoValidRange
     in
      onlyOneOfThisScript txInfoInputs thisValidator thisTxRef
        && case tsProposal of
          Trip travelAgentAddress travelerAddress totalTravelCost ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse "relative majority is too low" (relativeMajority >= dcTripRelativeMajorityPercent)
                  && traceIfFalse "majority is too small" (majorityPercent >= dcTripMajorityPercent)

              -- Get the disbursed amount
              disbursedAmount :: Value
              !disbursedAmount = singleton adaSymbol adaToken (min dcMaxTripDisbursement totalTravelCost)

              travelAgentLovelaces :: Integer
              !travelAgentLovelaces = (totalTravelCost * dcAgentDisbursementPercent) `divide` 1000

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
          General generalPaymentAddress generalPaymentValue ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse "relative majority is too low" (relativeMajority >= dcGeneralRelativeMajorityPercent)
                  && traceIfFalse "majority is too small" (majorityPercent >= dcGeneralMajorityPercent)

              -- Get the disbursed amount
              disbursedAmount :: Value
              !disbursedAmount = singleton adaSymbol adaToken (min dcMaxGeneralDisbursement generalPaymentValue)

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
          Upgrade upgradeMinter ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse "relative majority is too low" (relativeMajority >= dcUpgradeRelativeMajorityPercent)
                  && traceIfFalse "majority is too small" (majorityPercent >= dcUpgradeMajorityPercent)

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
  ValidatorHash ->
  [TxOut] ->
  [TxOut]
getContinuingOutputs' vh =
  filter
    ( \TxOut {..} ->
        addressCredential txOutAddress
          == ScriptCredential vh
    )

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

onlyOneOfThisScript :: [TxInInfo] -> ValidatorHash -> TxOutRef -> Bool
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

treasuryValidator :: TreasuryValidatorConfig -> Validator
treasuryValidator config = mkValidatorWithSettings compiledCode False
  where
    wrapValidateTreasury = wrapValidate validateTreasury
    compiledCode = $$(PlutusTx.compile [||wrapValidateTreasury||]) `applyCode` liftCode config

treasuryValidatorHash :: TreasuryValidatorConfig -> ValidatorHash
treasuryValidatorHash = validatorHash . treasuryValidator

treasuryScript :: TreasuryValidatorConfig -> PlutusScript PlutusScriptV2
treasuryScript = validatorToScript treasuryValidator
