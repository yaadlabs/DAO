{- |
Module: Dao.Vote
Description: Contains all the voting specific types.
-}
module Dao.Vote (
  -- * Datum
  VoteDatum (..),

  -- * Redeemers
  VoteMinterActionRedeemer (..),
  VoteActionRedeemer (..),

  -- * General vote related types
  VoteDirection (..),
) where

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V1.Value (TokenName)
import PlutusTx (unstableMakeIsData)
import PlutusTx.Prelude (
  Bool (False, True),
  Integer,
  (==),
 )
import PlutusTx.Prelude qualified as PlutusTx

-- | Vote direction
data VoteDirection
  = -- | Vote in favour of the proposal
    For
  | -- | Vote against the proposal
    Against

instance PlutusTx.Eq VoteDirection where
  For == For = True
  Against == Against = True
  _ == _ = False

-- | The vote datum, representing a vote cast by a user on a specific proposal
data VoteDatum = VoteDatum
  { vProposalTokenName :: TokenName
  -- ^ The name of the proposal for which this vote relates to.
  -- This is checked in 'Dao.Tally.Script.validateTally' to ensure
  -- the vote is for the correct proposal.
  , vDirection :: VoteDirection
  -- ^ Whether the vote is for or against the proposal.
  , vOwner :: Address
  -- ^ The address of the user casting the vote.
  , vReturnAda :: Integer
  -- ^ Ada amount to return
  }

-- | Redeemer for 'Dao.Vote.Script.mkVoteMinter' policy
data VoteMinterActionRedeemer = Mint | Burn

unstableMakeIsData ''VoteDirection
unstableMakeIsData ''VoteMinterActionRedeemer
unstableMakeIsData ''VoteDatum

-- | Redeemer for 'Dao.Vote.Script.validateVote' validator
data VoteActionRedeemer
  = -- | Vote should be counted in tallying phase
    Count
  | -- | Retract the vote
    Cancel

unstableMakeIsData ''VoteActionRedeemer
