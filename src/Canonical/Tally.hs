module Canonical.Tally where
import           Cardano.Api.Shelley (PlutusScript(..), PlutusScriptV2)
import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
-- import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           PlutusTx
import qualified PlutusTx.AssocMap as M
-- import           PlutusTx.AssocMap (Map)
import           PlutusTx.Prelude
-- import qualified Plutonomy
import           Canonical.Shared
-- TODO use the Tally NFT index
-- When making the Tally utxo it needs to make an NFT, that is
-- in the proposal datum, and the proposal utxo is in the tally datum
-- the proposal needs to stay locked
-- I think I should probably combine the proposals and the tallys
-- so there is only one utxo
-- Do I even need an NFT in that case?
-- Yes I need an NFT for voting
-- So the idea is mint a regular NFT
-- Put in on the proposal/tally
-- Votes refer to it
-- The NFT is special
-- It validates that the Tally has been initialize to zero
-- and is on the tally validator
-- How to make the NFT?
-- The token name is unique ... doesn't really matter
-- It is witness not a nft
-- What happens if there are two Tallys with the same witness?
-- You can split the votes
-- It should be unique actually
-- Can a collector ignore votes? Yes, but the voter can try to get their
-- votes in
-- So there is an index NFT that is used to make the tokenname
-- When validating you just need to make sure the policy correct

data IndexNftDatum = IndexNftDatum
  { indIndex :: Integer
  }

data IndexNftConfig = IndexNftConfig
  { incInitialUtxo :: TxOutRef
  , incTokenName   :: TokenName
  }

unstableMakeIsData ''IndexNftDatum
makeLift ''IndexNftConfig

mkNftMinter :: IndexNftConfig -> BuiltinData -> ScriptContext -> Bool
mkNftMinter IndexNftConfig {..} _ ScriptContext
  { scriptContextTxInfo = TxInfo {..}
  , scriptContextPurpose = Minting thisCurrencySymbol
  } =
  let
    hasWitness :: Value -> Bool
    hasWitness (Value v) = case M.lookup thisCurrencySymbol v of
      Just m -> case M.toList m of
        [(_, c)] -> if c == 1 then True else traceError "wrong token count"
        _ -> traceError "wrong number of tokens with policy id"
      _ -> False

    hasUTxO :: Bool
    !hasUTxO = any (\i -> txInInfoOutRef i == incInitialUtxo) txInfoInputs

    !IndexNftDatum{..} = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
      [ TxOut { txOutDatum } ] -> convertDatum txInfoData txOutDatum
      _ -> traceError "Impossible. No minted output."

    initialIndexIsZero :: Bool
    !initialIndexIsZero = indIndex == 0

    onlyOneTokenMinted :: Bool
    !onlyOneTokenMinted =
      hasSingleToken
        txInfoMint
        thisCurrencySymbol
        incTokenName

  in traceIfFalse "Missing significant UTxO!" hasUTxO
  && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted
  && traceIfFalse "Initial Index is not zero" initialIndexIsZero

mkNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: IndexNftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkNftMinter config a (unsafeFromBuiltinData b))

policy :: IndexNftConfig -> MintingPolicy
policy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicy c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

plutusScript :: IndexNftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: IndexNftConfig -> Validator
validator = Validator . plutusScript

tallyIndexNftMinterPolicyId :: IndexNftConfig -> CurrencySymbol
tallyIndexNftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: IndexNftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

tallyIndexNftMinter :: IndexNftConfig -> PlutusScript PlutusScriptV2
tallyIndexNftMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . scriptAsCbor
