module Scripts.Compile (CompileOpts (..), CompileMode (..), compile) where

import Prelude (IO, Eq, FilePath, Read, Show, print, return, ($), (.), (>>=))
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS (fromShort)
import Dao.Configuration.Script (configPolicyUnappliedCompiledCode, configValidatorUnappliedCompiledCode)
import Dao.Index.Script (indexPolicyUnappliedCompiledCode, indexValidatorCompiledCode)
import Dao.Vote.Script (votePolicyUnappliedCompiledCode, voteValidatorUnappliedCompiledCode)
import Dao.Tally.Script (tallyPolicyUnappliedCompiledCode, tallyValidatorUnappliedCompiledCode)
import Dao.Treasury.Script (treasuryValidatorUnappliedCompiledCode)
import LambdaBuffers.ApplicationConfig.Scripts 
  (Scripts 
    (Scripts
    , scripts'configPolicy
    , scripts'configValidator
    , scripts'indexPolicy
    , scripts'indexValidator
    , scripts'tallyPolicy
    , scripts'tallyValidator
    , scripts'votePolicy
    , scripts'voteValidator
    , scripts'treasuryValidator
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
            { scripts'configPolicy = Script (scriptToCborOptimised configPolicyUnappliedCompiledCode)
            , scripts'configValidator = Script (scriptToCborOptimised configValidatorUnappliedCompiledCode)
            , scripts'indexPolicy = Script (scriptToCborOptimised indexPolicyUnappliedCompiledCode)
            , scripts'indexValidator = Script (scriptToCborOptimised indexValidatorCompiledCode)
            , scripts'tallyPolicy = Script (scriptToCborOptimised tallyPolicyUnappliedCompiledCode)
            , scripts'tallyValidator = Script (scriptToCborOptimised tallyValidatorUnappliedCompiledCode)
            , scripts'votePolicy = Script (scriptToCborOptimised votePolicyUnappliedCompiledCode)
            , scripts'voteValidator = Script (scriptToCborOptimised voteValidatorUnappliedCompiledCode)
            , scripts'treasuryValidator = Script (scriptToCborOptimised treasuryValidatorUnappliedCompiledCode)
            }
  BS.writeFile (co'File opts) scripts
  
scriptToCborOptimised :: forall a. CompiledCode a -> BS.ByteString
scriptToCborOptimised = scriptToCbor . optimiseScript

scriptToCbor :: forall a. CompiledCode a -> BS.ByteString
scriptToCbor = SBS.fromShort . serialiseCompiledCode

optimiseScript :: forall a. CompiledCode a -> CompiledCode a
optimiseScript = optimizeUPLCWith aggressiveOptimizerOptions
