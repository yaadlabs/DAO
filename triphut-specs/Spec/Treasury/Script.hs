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
import Spec.SpecUtils (mkTypedValidator')
import Spec.Treasury.SampleData (sampleTreasuryValidatorConfig)
import Triphut.Treasury.Script (treasuryValidator, treasuryValidatorHash)

-- Validator script and info
type TreasuryValidatorScript = TypedValidator () ()

treasuryTypedValidator :: TreasuryValidatorScript
treasuryTypedValidator = mkTypedValidator' sampleTreasuryValidatorConfig treasuryValidator

treasuryValidatorHash' :: ValidatorHash
treasuryValidatorHash' = treasuryValidatorHash sampleTreasuryValidatorConfig
