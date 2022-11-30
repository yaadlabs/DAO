module Canonical.Types where

data TallyState = TallyState
  { tsProposal :: TxOutRef
  , tsFor      :: Integer
  , tsAgainst  :: Integer
  }

data UpgradeProposal = UpgradeProposal
  { upUpgradeMinter :: CurrencySymbol
  }
