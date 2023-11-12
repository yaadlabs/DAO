{- |
Module      : Spec.Vote.Context
Description : Vote policy context unit tests
-}
module Spec.Vote.Context (validVoteConfigNftTest) where

import Control.Monad (void)
import Plutus.Model (
  Run,
  TypedPolicy,
  adaValue,
  mintValue,
  newUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToScript,
  refInputInline,
  spendScript,
 )
import Plutus.V1.Ledger.Value (TokenName (TokenName), Value, singleton)
import PlutusTx.Prelude (($))
import Spec.ConfigurationNft.Transactions (runInitConfig)
import Spec.ConfigurationNft.Utils (findConfig)
import Spec.Index.Script (indexNftTypedValidator)
import Spec.Index.Transactions (runInitIndex)
import Spec.Index.Utils (findIndex)
import Spec.SpecUtils (minAda)
import Spec.Values (
  dummyConfigNftSymbol,
  dummyConfigNftTokenName,
  dummyIndexConfigNftSymbol,
  dummyIndexConfigNftTokenName,
  dummyIndexConfigNftValue,
 )
import Spec.Vote.SampleData (sampleVoteDatum)
import Spec.Vote.Script (
  VoteMintingPolicy,
  VoteValidatorScript,
  voteCurrencySymbol,
  voteMintingPolicy,
  voteTypedMintingPolicy,
  voteTypedValidator,
  voteValidatorHash',
 )
import Spec.Vote.Transactions (runInitVoteConfig)
import Spec.Vote.Utils (findVote)
import Triphut.Index (IndexNftDatum (IndexNftDatum))
import Triphut.Vote (VoteMinterConfig (VoteMinterConfig))
import Prelude (mconcat, pure, (+), (<>))

validVoteConfigNftTest :: Run ()
validVoteConfigNftTest = do
  void runInitConfig
  void runInitIndex
  void runInitVoteConfig

  (configOutRef, _, _) <- findConfig
  (indexOutRef, _, indexDatum) <- findIndex
  (voteOutRef, _, voteDatum) <- findVote

  pure ()
