module Spec.Addresses (sampleVoterAddress) where

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
