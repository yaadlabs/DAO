module Spec.Addresses (
  dummyVoterAddress,
  dummyTravelAgentAddress,
  dummyTravelerPaymentAddress,
  dummyGeneralPaymentAddress,
) where

import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Bytes (getLedgerBytes)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (PubKeyHash))
import PlutusTx.Prelude (($))

-- | Sample voter address
dummyVoterAddress :: Address
dummyVoterAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "00000000000000000000000000000000000000000000000000000000"

dummyTravelerPaymentAddress :: Address
dummyTravelerPaymentAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "11111111111111111111111111111111111111111111111111111111"

dummyTravelAgentAddress :: Address
dummyTravelAgentAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "22222222222222222222222222222222222222222222222222222222"

dummyGeneralPaymentAddress :: Address
dummyGeneralPaymentAddress =
  pubKeyHashAddress $
    PubKeyHash $
      getLedgerBytes "33333333333333333333333333333333333333333333333333333333"
