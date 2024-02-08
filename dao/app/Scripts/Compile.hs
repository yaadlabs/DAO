module Scripts.Compile (CompileOpts (..), CompileMode (..), compile) where

import Dao.Configuration.Script (
  configPolicyCompiledCode,
  configValidatorCompiledCode,
 )
import Dao.Index.Script (indexPolicyCompiledCode, indexValidatorCompiledCode)
import Dao.Tally.Script (tallyPolicyCompiledCode, tallyValidatorCompiledCode)
import Dao.Treasury.Script (treasuryPolicyCompiledCode, treasuryValidatorCompiledCode)
import Dao.Vote.Script (
  fungiblePolicyCompiledCode,
  voteNftPolicyCompiledCode,
  votePolicyCompiledCode,
  voteValidatorCompiledCode,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS (fromShort)
import LambdaBuffers.ApplicationConfig.Scripts (
  Script (Script),
  Scripts (
    Scripts,
    scripts'configPolicy,
    scripts'configPolicyDebug,
    scripts'configValidator,
    scripts'configValidatorDebug,
    scripts'fungiblePolicy,
    scripts'indexPolicy,
    scripts'indexPolicyDebug,
    scripts'indexValidator,
    scripts'indexValidatorDebug,
    scripts'tallyPolicy,
    scripts'tallyPolicyDebug,
    scripts'tallyValidator,
    scripts'tallyValidatorDebug,
    scripts'treasuryPolicy,
    scripts'treasuryPolicyDebug,
    scripts'treasuryValidator,
    scripts'treasuryValidatorDebug,
    scripts'voteNftPolicy,
    scripts'voteNftPolicyDebug,
    scripts'votePolicy,
    scripts'votePolicyDebug,
    scripts'voteValidator,
    scripts'voteValidatorDebug
  ),
 )
import LambdaBuffers.Runtime.Prelude (toJsonBytes)
import Plutonomy
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import PlutusTx (CompiledCode)
import PlutusTx.Plugin ()
import Prelude (Eq, FilePath, IO, Read, Show, ($), (.))

data CompileMode = COMPILE_PROD | COMPILE_DEBUG deriving stock (Show, Read, Eq)

data CompileOpts = CompileOpts
  { co'Mode :: CompileMode
  , co'File :: FilePath
  }
  deriving stock (Show, Eq)

compile :: CompileOpts -> IO ()
compile opts = do
  let scripts =
        toJsonBytes $
          Scripts
            { scripts'configPolicy = Script (scriptToCborOptimised configPolicyCompiledCode)
            , scripts'configPolicyDebug = Script (scriptToCbor configPolicyCompiledCode)
            , scripts'configValidator = Script (scriptToCborOptimised configValidatorCompiledCode)
            , scripts'configValidatorDebug = Script (scriptToCbor configValidatorCompiledCode)
            , scripts'indexPolicy = Script (scriptToCborOptimised indexPolicyCompiledCode)
            , scripts'indexPolicyDebug = Script (scriptToCbor indexPolicyCompiledCode)
            , scripts'indexValidator = Script (scriptToCborOptimised indexValidatorCompiledCode)
            , scripts'indexValidatorDebug = Script (scriptToCbor indexValidatorCompiledCode)
            , scripts'tallyPolicy = Script (scriptToCborOptimised tallyPolicyCompiledCode)
            , scripts'tallyPolicyDebug = Script (scriptToCbor tallyPolicyCompiledCode)
            , scripts'tallyValidator = Script (scriptToCborOptimised tallyValidatorCompiledCode)
            , scripts'tallyValidatorDebug = Script (scriptToCbor tallyValidatorCompiledCode)
            , scripts'votePolicy = Script (scriptToCborOptimised votePolicyCompiledCode)
            , scripts'votePolicyDebug = Script (scriptToCbor votePolicyCompiledCode)
            , scripts'voteValidator = Script (scriptToCborOptimised voteValidatorCompiledCode)
            , scripts'voteValidatorDebug = Script (scriptToCbor voteValidatorCompiledCode)
            , scripts'treasuryValidator = Script (scriptToCborOptimised treasuryValidatorCompiledCode)
            , scripts'treasuryValidatorDebug = Script (scriptToCbor treasuryValidatorCompiledCode)
            , scripts'treasuryPolicyDebug = Script (scriptToCbor treasuryPolicyCompiledCode)
            , scripts'treasuryPolicy = Script (scriptToCborOptimised treasuryPolicyCompiledCode)
            , scripts'fungiblePolicy = Script (scriptToCborOptimised fungiblePolicyCompiledCode)
            , scripts'voteNftPolicy = Script (scriptToCborOptimised voteNftPolicyCompiledCode)
            , scripts'voteNftPolicyDebug = Script (scriptToCbor voteNftPolicyCompiledCode)
            }
  BS.writeFile (co'File opts) scripts

scriptToCborOptimised :: forall a. CompiledCode a -> BS.ByteString
scriptToCborOptimised = scriptToCbor . optimiseScript

scriptToCbor :: forall a. CompiledCode a -> BS.ByteString
scriptToCbor = SBS.fromShort . serialiseCompiledCode

optimiseScript :: forall a. CompiledCode a -> CompiledCode a
optimiseScript = optimizeUPLCWith aggressiveOptimizerOptions
