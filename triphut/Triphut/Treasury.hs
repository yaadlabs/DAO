module Triphut.Treasury (
  TreasuryValidatorConfig (..),
  treasuryScript,
  treasuryValidatorHash,
) where

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Plutonomy qualified
import Plutus.V1.Ledger.Address (Address (Address, addressCredential))
import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval (before)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, Validator, ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime), POSIXTimeRange)
import Plutus.V1.Ledger.Value as V
import Plutus.V2.Ledger.Tx hiding (Mint)
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
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Integer,
  Maybe (Just, Nothing),
  check,
  divide,
  filter,
  mapMaybe,
  mconcat,
  min,
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
  validatorHash,
 )
import Triphut.Types (
  DynamicConfig (
    DynamicConfig,
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
    dcUpgradRelativeMajorityPercent,
    dcUpgradeMajorityPercent
  ),
  ProposalType (General, Trip, Upgrade),
  TallyState (TallyState, tsAgainst, tsFor, tsProposal, tsProposalEndTime),
 )

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

data TreasuryTxOut = TreasuryTxOut
  { tTxOutAddress :: Address
  , tTxOutValue :: Value
  , tTxOutDatum :: OutputDatum
  , tTxOutReferenceScript :: BuiltinData
  }

data TreasuryTxInInfo = TreasuryTxInInfo
  { tTxInInfoOutRef :: TxOutRef
  , tTxInInfoResolved :: TreasuryTxOut
  }

newtype TreasuryScriptPurpose = TreasurySpend TxOutRef

data TreasuryScriptContext = TreasuryScriptContext
  { tScriptContextTxInfo :: TreasuryTxInfo
  , tScriptContextPurpose :: TreasuryScriptPurpose
  }

data TreasuryTxInfo = TreasuryTxInfo
  { tTxInfoInputs :: [TreasuryTxInInfo]
  , tTxInfoReferenceInputs :: [TreasuryTxInInfo]
  , tTxInfoOutputs :: [TreasuryTxOut]
  , tTxInfoFee :: BuiltinData
  , tTxInfoMint :: Value
  , tTxInfoDCert :: BuiltinData
  , tTxInfoWdrl :: BuiltinData
  , tTxInfoValidRange :: POSIXTimeRange
  , tTxInfoSignatories :: [PubKeyHash]
  , tTxInfoRedeemers :: BuiltinData
  , tTxInfoData :: Map DatumHash Datum
  , tTxInfoId :: BuiltinData
  }

-------------------------------------------------------------------------------
-- Input Types
-------------------------------------------------------------------------------

type Treasury = BuiltinData

data TreasuryValidatorConfig = TreasuryValidatorConfig
  { tvcConfigNftCurrencySymbol :: CurrencySymbol
  , tvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''TreasuryTxOut
unstableMakeIsData ''TreasuryTxInInfo
makeIsDataIndexed ''TreasuryScriptPurpose [('TreasurySpend, 1)]
unstableMakeIsData ''TreasuryScriptContext
unstableMakeIsData ''TreasuryTxInfo
makeLift ''TreasuryValidatorConfig

addressOutputsAt :: Address -> [TreasuryTxOut] -> [Value]
addressOutputsAt addr outs =
  let
    flt TreasuryTxOut {tTxOutAddress, tTxOutValue}
      | addr == tTxOutAddress = Just tTxOutValue
      | otherwise = Nothing
   in
    mapMaybe flt outs

valuePaidTo' :: [TreasuryTxOut] -> Address -> Value
valuePaidTo' outs addr = mconcat (addressOutputsAt addr outs)

getContinuingOutputs' ::
  ValidatorHash ->
  [TreasuryTxOut] ->
  [TreasuryTxOut]
getContinuingOutputs' vh =
  filter
    ( \TreasuryTxOut {..} ->
        addressCredential tTxOutAddress
          == ScriptCredential vh
    )

ownValueAndValidator :: [TreasuryTxInInfo] -> TxOutRef -> (Value, ValidatorHash)
ownValueAndValidator ins txOutRef = go ins
  where
    go = \case
      [] -> traceError "The impossible happened"
      TreasuryTxInInfo {tTxInInfoOutRef, tTxInInfoResolved = TreasuryTxOut {tTxOutAddress = Address {..}, ..}} : xs ->
        if tTxInInfoOutRef == txOutRef
          then case addressCredential of
            ScriptCredential vh -> (tTxOutValue, vh)
            _ -> traceError "Impossible. Expected ScriptCredential"
          else go xs

onlyOneOfThisScript :: [TreasuryTxInInfo] -> ValidatorHash -> TxOutRef -> Bool
onlyOneOfThisScript ins vh expectedRef = go ins
  where
    go = \case
      [] -> True
      TreasuryTxInInfo {tTxInInfoOutRef, tTxInInfoResolved = TreasuryTxOut {tTxOutAddress = Address {..}}} : xs ->
        if isScriptCredential addressCredential && tTxInInfoOutRef /= expectedRef
          then case addressCredential of
            ScriptCredential vh' | vh' == vh -> False
            _ -> go xs
          else go xs

validateTreasury ::
  TreasuryValidatorConfig ->
  Treasury ->
  BuiltinData ->
  TreasuryScriptContext ->
  Bool
validateTreasury
  TreasuryValidatorConfig {..}
  _treasury
  _action
  TreasuryScriptContext
    { tScriptContextTxInfo = TreasuryTxInfo {..}
    , tScriptContextPurpose = TreasurySpend thisTxRef
    } =
    let
      -- check that there is only one of this script
      inputValue :: Value
      thisValidator :: ValidatorHash

      (!inputValue, !thisValidator) = ownValueAndValidator tTxInfoInputs thisTxRef

      hasConfigurationNft :: Value -> Bool
      hasConfigurationNft = hasOneOfToken tvcConfigNftCurrencySymbol tvcConfigNftTokenName

      hasTallyNft :: Value -> Bool
      hasTallyNft = hasSymbolInValue dcTallyNft

      -- filter the reference inputs for the configuration nft
      DynamicConfig {..} = case filter (hasConfigurationNft . tTxOutValue . tTxInInfoResolved) tTxInfoReferenceInputs of
        [TreasuryTxInInfo {tTxInInfoResolved = TreasuryTxOut {..}}] -> convertDatum tTxInfoData tTxOutDatum
        _ -> traceError "Too many NFT values"

      TallyState {..} = case filter (hasTallyNft . tTxOutValue . tTxInInfoResolved) tTxInfoReferenceInputs of
        [] -> traceError "Missing tally NFT"
        [TreasuryTxInInfo {tTxInInfoResolved = TreasuryTxOut {..}}] -> convertDatum tTxInfoData tTxOutDatum
        _ -> traceError "Too many NFT values"

      totalVotes :: Integer
      !totalVotes = tsFor + tsAgainst

      relativeMajority :: Integer
      !relativeMajority = (totalVotes * 1000) `divide` dcTotalVotes

      majorityPercent :: Integer
      !majorityPercent = (tsFor * 1000) `divide` totalVotes

      isAfterTallyEndTime :: Bool
      !isAfterTallyEndTime = (tsProposalEndTime + POSIXTime dcProposalTallyEndOffset) `before` tTxInfoValidRange
     in
      onlyOneOfThisScript tTxInfoInputs thisValidator thisTxRef
        && case tsProposal of
          Trip travelAgentAddress travelerAddress totalTravelCost ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse "relative majority is too low" (relativeMajority >= dcTripRelativeMajorityPercent)
                  && traceIfFalse "majority is too small" (majorityPercent >= dcTripMajorityPercent)

              -- Get the disbursed amount
              disbursedAmount :: Value
              !disbursedAmount = V.singleton adaSymbol adaToken (min dcMaxTripDisbursement totalTravelCost)

              travelAgentLovelaces :: Integer
              !travelAgentLovelaces = (totalTravelCost * dcAgentDisbursementPercent) `divide` 1000

              travelerLovelaces :: Integer
              !travelerLovelaces = totalTravelCost - travelAgentLovelaces

              -- Make sure the disbursed amount is less than the max
              -- Find the total value returned to the script address
              outputValue :: Value
              !outputValue = case getContinuingOutputs' thisValidator tTxInfoOutputs of
                [TreasuryTxOut {..}] -> tTxOutValue
                _ -> traceError "expected exactly one continuing output"

              outputValueIsLargeEnough :: Bool
              !outputValueIsLargeEnough = outputValue `geq` (inputValue - disbursedAmount)

              -- Paid the ptGeneralPaymentAddress the ptGeneralPaymentValue
              paidToTravelAgentAddress :: Bool
              !paidToTravelAgentAddress =
                lovelacesOf (valuePaidTo' tTxInfoOutputs travelAgentAddress) >= travelAgentLovelaces

              paidToTravelerAddress :: Bool
              !paidToTravelerAddress = lovelacesOf (valuePaidTo' tTxInfoOutputs travelerAddress) >= travelerLovelaces
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
              !disbursedAmount = V.singleton adaSymbol adaToken (min dcMaxGeneralDisbursement generalPaymentValue)

              -- Make sure the disbursed amount is less than the max
              -- Find the total value returned to the script address
              outputValue :: Value
              !outputValue = case getContinuingOutputs' thisValidator tTxInfoOutputs of
                [TreasuryTxOut {..}] -> tTxOutValue
                _ -> traceError "expected exactly one continuing output"

              outputValueIsLargeEnough :: Bool
              !outputValueIsLargeEnough = outputValue `geq` (inputValue - disbursedAmount)

              -- Paid the ptGeneralPaymentAddress the ptGeneralPaymentValue
              paidToAddress :: Bool
              !paidToAddress = lovelacesOf (valuePaidTo' tTxInfoOutputs generalPaymentAddress) >= generalPaymentValue
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && traceIfFalse "Disbursing too much" outputValueIsLargeEnough
                && traceIfFalse "Not paying to the correct address" paidToAddress
          Upgrade upgradeMinter ->
            let
              hasEnoughVotes :: Bool
              !hasEnoughVotes =
                traceIfFalse "relative majority is too low" (relativeMajority >= dcUpgradRelativeMajorityPercent)
                  && traceIfFalse "majority is too small" (majorityPercent >= dcUpgradeMajorityPercent)

              -- Make sure the upgrade token was minted
              hasUpgradeMinterToken :: Bool
              !hasUpgradeMinterToken = hasTokenInValue upgradeMinter "Treasury Minter" tTxInfoMint
             in
              traceIfFalse "The proposal doesn't have enough votes" hasEnoughVotes
                && traceIfFalse "Not minting upgrade token" hasUpgradeMinterToken
                && traceIfFalse "Tallying not over. Try again later" isAfterTallyEndTime

wrapValidateTreasury ::
  TreasuryValidatorConfig ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrapValidateTreasury cfg x y z =
  check
    ( validateTreasury
        cfg
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    )

treasuryValidator :: TreasuryValidatorConfig -> Validator
treasuryValidator cfg =
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
          $$(PlutusTx.compile [||wrapValidateTreasury||])
            `applyCode` liftCode cfg

treasuryValidatorHash :: TreasuryValidatorConfig -> ValidatorHash
treasuryValidatorHash = validatorHash . treasuryValidator

treasuryScript :: TreasuryValidatorConfig -> PlutusScript PlutusScriptV2
treasuryScript =
  PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise
    . treasuryValidator
