module Spec.Vote.Transactions (
  runInitVoteMinterConfig,
  runInitVoteConfig,
  runInitVote,
  runInitVoteWithUser,
) where

import Plutus.Model (
  Run,
  getMainUser,
  spend,
  submitTx,
  userSpend,
 )
import Plutus.Model.V2 (
  DatumMode (InlineDatum),
  payToRef,
  payToScript,
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import PlutusTx.Prelude (($))
import Spec.AlwaysSucceed.Script (alwaysSucceedTypedValidator1, alwaysSucceedTypedValidator3)
import Spec.SampleData (sampleVoteDynamicConfig, sampleVoteMinterDynamicConfig)
import Spec.SpecUtils (initScriptRef, minAda, runInitPayToScript, runInitReferenceScript)
import Spec.Values (dummyVoteConfigNftValue, dummyVoteValue)
import Spec.Vote.SampleData (sampleVoteDatum, sampleVoteDatumWithUser)
import Spec.Vote.Script (voteTypedValidator)
import Prelude ((<>))

runInitVoteConfig :: Run ()
runInitVoteConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator3
    sampleVoteDynamicConfig
    (dummyVoteConfigNftValue <> minAda)

runInitVoteMinterConfig :: Run ()
runInitVoteMinterConfig =
  runInitReferenceScript
    alwaysSucceedTypedValidator1
    sampleVoteMinterDynamicConfig
    (dummyVoteConfigNftValue <> minAda)

runInitVote :: Run ()
runInitVote =
  runInitPayToScript
    voteTypedValidator
    sampleVoteDatum
    dummyVoteValue

runInitVoteWithUser :: PubKeyHash -> Run ()
runInitVoteWithUser pkh =
  runInitPayToScript
    voteTypedValidator
    (sampleVoteDatumWithUser pkh)
    dummyVoteValue
