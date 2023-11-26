{- |
Module      : Spec.Treasury.Script
Description : Treasury script
-}
module Spec.Treasury.Script (
  TreasuryValidatorScript,
  treasuryTypedValidator,
  treasuryValidatorHash',
)
where

import Plutus.Model.V2 (TypedValidator)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Spec.ConfigurationNft.SampleData (sampleConfigValidatorConfig)
import Spec.SpecUtils (mkTypedValidator')
import Triphut.Treasury.Script (treasuryValidator, treasuryValidatorHash)

-- Validator script and info
type TreasuryValidatorScript = TypedValidator () ()

treasuryTypedValidator :: TreasuryValidatorScript
treasuryTypedValidator = mkTypedValidator' sampleConfigValidatorConfig treasuryValidator

treasuryValidatorHash' :: ValidatorHash
treasuryValidatorHash' = treasuryValidatorHash sampleConfigValidatorConfig
