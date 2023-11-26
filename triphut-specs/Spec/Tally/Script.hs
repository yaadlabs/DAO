{- |
Module      : Spec.Tally.Script
Description : Tally scripts
-}
module Spec.Tally.Script (
  tallyConfigNftTypedMintingPolicy,
  tallyNftTypedValidator,
  tallyConfigNftValue,
  tallyConfigNftCurrencySymbol,
  tallyValidatorHash',
)
where

import Plutus.Model.V2 (
  TypedPolicy,
  TypedValidator,
  mkTypedPolicy,
  scriptCurrencySymbol,
  toBuiltinPolicy,
 )
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, Value, singleton)
import PlutusTx qualified
import PlutusTx.Prelude (($), (.))
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')
import Triphut.Tally (TallyNftConfig (TallyNftConfig))
import Triphut.Tally.Script (mkTallyNftMinter, tallyValidator, tallyValidatorHash)
import Triphut.Types (TallyStateDatum)

-- Policy script and info
tallyConfigNftTypedMintingPolicy :: TallyNftConfig -> TypedPolicy ()
tallyConfigNftTypedMintingPolicy config =
  mkTypedPolicy $
    $$(PlutusTx.compile [||toBuiltinPolicy . mkTallyNftMinter||])
      `PlutusTx.applyCode` PlutusTx.liftCode config

tallyConfigNftCurrencySymbol :: TallyNftConfig -> CurrencySymbol
tallyConfigNftCurrencySymbol = scriptCurrencySymbol . tallyConfigNftTypedMintingPolicy

tallyConfigNftValue :: TallyNftConfig -> Value
tallyConfigNftValue nftCfg@(TallyNftConfig _ tokenName _ _) =
  singleton (tallyConfigNftCurrencySymbol nftCfg) tokenName 1

-- Validator script and info
type TallyValidatorScript = TypedValidator TallyStateDatum ()

tallyNftTypedValidator :: TallyValidatorScript
tallyNftTypedValidator = mkTypedValidator' sampleConfigValidatorConfig tallyValidator

tallyValidatorHash' :: ValidatorHash
tallyValidatorHash' = tallyValidatorHash sampleConfigValidatorConfig
