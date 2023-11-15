module Spec.Addresses (
  sampleVoterAddress,
  sampleTravelAgentAddress,
  sampleTravelerPaymentAddress,
  sampleGeneralPaymentAddress,
) where

import Plutus.V1.Ledger.Address (Address, pubKeyHashAddress)
import Plutus.V1.Ledger.Bytes (getLedgerBytes)
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
import PlutusTx.Prelude (($))

-- | Sample voter address
sampleVoterAddress :: Address
sampleVoterAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "00000000000000000000000000000000000000000000000000000000"

sampleTravelerPaymentAddress :: Address
sampleTravelerPaymentAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "11111111111111111111111111111111111111111111111111111111"

sampleTravelAgentAddress :: Address
sampleTravelAgentAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "22222222222222222222222222222222222222222222222222222222"

sampleGeneralPaymentAddress :: Address
sampleGeneralPaymentAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "33333333333333333333333333333333333333333333333333333333"
