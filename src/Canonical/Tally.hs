module Canonical.Tally where

-- TODO use the Tally NFT index
-- When making the Tally utxo it needs to make an NFT, that is
-- in the proposal datum, and the proposal utxo is in the tally datum
-- the proposal needs to stay locked
-- I think I should probably combine the proposals and the tallys
-- so there is only one utxo
-- Do I even need an NFT in that case?

data NftConfig = NftConfig
  { ncInitialUtxo :: TxOutRef
  , ncTokenName   :: TokenName
  }

makeLift ''NftConfig

mkNftMinter :: NftConfig -> BuiltinData -> ScriptContext -> Bool
mkNftMinter NftConfig {..} _ ScriptContext
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
    !hasUTxO = any (\i -> txInInfoOutRef i == ncInitialUtxo) txInfoInputs

    -- This errors if more than one token is used as an output with this policy id
    _newOutput :: DynamicConfig
    !_newOutput = case filter (\TxOut {..} -> hasWitness txOutValue) txInfoOutputs of
      [ TxOut { txOutDatum } ] -> convertDatum txInfoData txOutDatum
      _ -> traceError "Impossible. No minted output."

    onlyOneTokenMinted :: Bool
    !onlyOneTokenMinted =
      hasSingleToken
        txInfoMint
        thisCurrencySymbol
        ncTokenName

  in traceIfFalse "Missing significant UTxO!" hasUTxO
  && traceIfFalse "Wrong mint amount!" onlyOneTokenMinted

mkNftMinter _ _ _ = traceError "wrong type of script purpose!"

wrappedPolicy :: NftConfig -> WrappedMintingPolicyType
wrappedPolicy config a b = check (mkNftMinter config a (unsafeFromBuiltinData b))

policy :: NftConfig -> MintingPolicy
policy cfg = mkMintingPolicyScript $
  $$(compile [|| \c -> wrappedPolicy c ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cfg

plutusScript :: NftConfig -> Script
plutusScript = unMintingPolicyScript . policy

validator :: NftConfig -> Validator
validator = Validator . plutusScript

nftMinterPolicyId :: NftConfig -> CurrencySymbol
nftMinterPolicyId = mpsSymbol . mintingPolicyHash . policy

scriptAsCbor :: NftConfig -> BSL.ByteString
scriptAsCbor = serialise . validator

nftMinter :: NftConfig -> PlutusScript PlutusScriptV2
nftMinter
  = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . scriptAsCbor
