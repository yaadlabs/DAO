module Scripts.Compile (CompileOpts (..), CompileMode (..), compile) where

import Prelude (IO, Eq, FilePath, Read, Show, print, return, ($), (.), (>>=))
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS (fromShort)
import Dao.Configuration.Script (configPolicyCompiledCode, configValidatorUnappliedCompiledCode, testValidatorCompiled, alwaysMintsCompiled)
import Dao.Index.Script (indexPolicyUnappliedCompiledCode, indexPolicyCompiledCode, indexValidatorCompiledCode, indexValidatorCompiledCode')
import Dao.Vote.Script (votePolicyUnappliedCompiledCode, voteValidatorUnappliedCompiledCode)
import Dao.Tally.Script (tallyPolicyUnappliedCompiledCode, tallyValidatorUnappliedCompiledCode, tallyPolicyCompiledCode)
import Dao.Treasury.Script (treasuryValidatorUnappliedCompiledCode)
import LambdaBuffers.ApplicationConfig.Scripts 
  (Scripts 
    (Scripts
    , scripts'alwaysMints
    , scripts'testValidator
    , scripts'configPolicy
    , scripts'configPolicyDebug
    , scripts'configValidator
    , scripts'configValidatorDebug
    , scripts'indexPolicy
    , scripts'indexPolicyDebug
    , scripts'indexValidator
    , scripts'indexValidatorDebug
    , scripts'tallyPolicy
    , scripts'tallyPolicyDebug
    , scripts'tallyValidator
    , scripts'tallyValidatorDebug
    , scripts'votePolicy
    , scripts'votePolicyDebug
    , scripts'voteValidator
    , scripts'voteValidatorDebug
    , scripts'treasuryValidator
    , scripts'treasuryValidatorDebug
    )
  , Script (Script)
  )
import LambdaBuffers.Runtime.Prelude (toJsonBytes)
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx.Plugin ()
import Dao.ScriptArgument (NftConfig)
import Plutonomy
import Data.Either (Either (Left, Right), either)
import Data.Maybe (Maybe (Just, Nothing))

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
            { -- scripts'configPolicy = Script (scriptToCborOptimised configPolicyUnappliedCompiledCode)
              scripts'testValidator = Script (scriptToCborOptimised testValidatorCompiled)
            , scripts'alwaysMints = Script (scriptToCborOptimised alwaysMintsCompiled)
            , scripts'configPolicy = Script (scriptToCborOptimised configPolicyCompiledCode)
            , scripts'configPolicyDebug = Script (scriptToCbor configPolicyCompiledCode)
            , scripts'configValidator = Script (scriptToCborOptimised configValidatorUnappliedCompiledCode)
            , scripts'configValidatorDebug = Script (scriptToCbor configValidatorUnappliedCompiledCode)
            , scripts'indexPolicy = Script (scriptToCborOptimised indexPolicyCompiledCode)
            , scripts'indexPolicyDebug = Script (scriptToCbor indexPolicyCompiledCode)
            -- , scripts'indexValidator = Script (scriptToCborOptimised indexValidatorCompiledCode')
            -- , scripts'indexPolicy = Script (scriptToCborOptimised indexPolicyUnappliedCompiledCode)
            , scripts'indexValidator = Script (scriptToCborOptimised indexValidatorCompiledCode)
            , scripts'indexValidatorDebug = Script (scriptToCbor indexValidatorCompiledCode)
            , scripts'tallyPolicy = Script (scriptToCborOptimised tallyPolicyCompiledCode)
            , scripts'tallyPolicyDebug = Script (scriptToCbor tallyPolicyCompiledCode)
            -- , scripts'tallyPolicy = Script (scriptToCborOptimised tallyPolicyUnappliedCompiledCode)
            , scripts'tallyValidator = Script (scriptToCborOptimised tallyValidatorUnappliedCompiledCode)
            , scripts'tallyValidatorDebug = Script (scriptToCbor tallyValidatorUnappliedCompiledCode)
            , scripts'votePolicy = Script (scriptToCborOptimised votePolicyUnappliedCompiledCode)
            , scripts'votePolicyDebug = Script (scriptToCbor votePolicyUnappliedCompiledCode)
            , scripts'voteValidator = Script (scriptToCborOptimised voteValidatorUnappliedCompiledCode)
            , scripts'voteValidatorDebug = Script (scriptToCbor voteValidatorUnappliedCompiledCode)
            , scripts'treasuryValidator = Script (scriptToCborOptimised treasuryValidatorUnappliedCompiledCode)
            , scripts'treasuryValidatorDebug = Script (scriptToCbor treasuryValidatorUnappliedCompiledCode)
            }
  BS.writeFile (co'File opts) scripts
  
scriptToCborOptimised :: forall a. CompiledCode a -> BS.ByteString
scriptToCborOptimised = scriptToCbor . optimiseScript

scriptToCbor :: forall a. CompiledCode a -> BS.ByteString
scriptToCbor = SBS.fromShort . serialiseCompiledCode

optimiseScript :: forall a. CompiledCode a -> CompiledCode a
optimiseScript = optimizeUPLCWith aggressiveOptimizerOptions
