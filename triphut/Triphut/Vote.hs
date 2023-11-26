{- |
Module: Triphut.Vote
Description: Contains all the voting specific types.
-}
module Triphut.Vote (
  -- * Datum
  VoteDatum (..),

  -- * Redeemers
  VoteMinterActionRedeemer (..),
  VoteActionRedeemer (..),

  -- * General vote related types
  VoteDirection (..),
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Value (TokenName)
import PlutusTx (unstableMakeIsData)
import PlutusTx.Prelude (
  Bool (False, True),
  Integer,
  (==),
 )
import PlutusTx.Prelude qualified as PlutusTx

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

-- | Redeemer for 'Triphut.Vote.Script.validateVote' validator
data VoteActionRedeemer
  = Count
  | Cancel

unstableMakeIsData ''VoteActionRedeemer
