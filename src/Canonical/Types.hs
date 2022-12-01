module Canonical.Types where
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           PlutusTx.Prelude
import           PlutusTx

data TallyState = TallyState
  { tsProposal :: TxOutRef
  , tsFor      :: Integer
  , tsAgainst  :: Integer
  }

data UpgradeProposal = UpgradeProposal
  { upUpgradeMinter :: CurrencySymbol
  , upEndTime       :: POSIXTime
  }

unstableMakeIsData ''TallyState
unstableMakeIsData ''UpgradeProposal
