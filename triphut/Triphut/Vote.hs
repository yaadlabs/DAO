{- |
Module: Triphut.Vote
Description: Contains all the voting specific types.
-}
module Triphut.Vote (
  -- * Datums
  VoteDatum (..),
  VoteMinterDynamicConfigDatum (..),
  VoteDynamicConfigDatum (..),

  -- * Redeemers
  VoteMinterActionRedeemer (..),
  VoteActionRedeemer (..),

  -- * General vote related types
  VoteDirection (..),

  -- * Script arguments, containing relevant CurrenySymbol and TokenName
  VoteMinterConfig (..),
  VoteValidatorConfig (..),
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.Prelude (
  Bool (False, True),
  BuiltinData,
  Integer,
  (==),
 )
import PlutusTx.Prelude qualified as PlutusTx

-- | 'Triphut.Vote.Script.mkVoteMinter' argument
data VoteMinterConfig = VoteMinterConfig
  { vmcConfigNftCurrencySymbol :: CurrencySymbol
  , vmcConfigNftTokenName :: TokenName
  }

makeLift ''VoteMinterConfig

-- | Vote direction
data VoteDirection = For | Against

instance PlutusTx.Eq VoteDirection where
  For == For = True
  Against == Against = True
  _ == _ = False

-- | The vote datum, represnting a vote cast by a user on a specific proposal
data VoteDatum = VoteDatum
  { vProposalTokenName :: TokenName
  -- ^ The name of the proposal for which this vote relates to.
  -- This is checked in 'Triphut.Tally.Script.validateTally' to ensure
  -- the vote is for the correct proposal.
  , vDirection :: VoteDirection
  -- ^ Whether the vote is for or against the proposal.
  , vOwner :: Address
  -- ^ The address of the user casting the vote.
  , vReturnAda :: Integer
  -- ^ Ada amount to return
  }

-- | Redeemer for 'Triphut.Vote.Script.mkVoteMinter' policy
data VoteMinterActionRedeemer = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterActionRedeemer
unstableMakeIsData ''VoteDatum

-- | Vote minter config datum, representation mirrors the main 'Triphut.Types.DynamicConfigDatum'
data VoteMinterDynamicConfigDatum = VoteMinterDynamicConfigDatum
  { vmdcTallyNft :: CurrencySymbol
  , vmdcVoteTokenName :: TokenName
  , vmdcVoteValidator :: ValidatorHash
  , vmdcVoteNft :: CurrencySymbol
  }

-- | Vote config datum, representation mirrors the main 'Triphut.Types.DynamicConfigDatum'
data VoteDynamicConfigDatum = VoteDynamicConfigDatum
  { vdcTallyValidator :: ValidatorHash
  , vdcVoteCurrencySymbol :: BuiltinData
  }

unstableMakeIsData ''VoteMinterDynamicConfigDatum

-- | Redeemer for 'Triphut.Vote.Script.validateVote' validator
data VoteActionRedeemer
  = Count
  | Cancel

data VoteValidatorConfig = VoteValidatorConfig
  { vvcConfigNftCurrencySymbol :: CurrencySymbol
  , vvcConfigNftTokenName :: TokenName
  }

unstableMakeIsData ''VoteActionRedeemer
unstableMakeIsData ''VoteDynamicConfigDatum
makeLift ''VoteValidatorConfig
