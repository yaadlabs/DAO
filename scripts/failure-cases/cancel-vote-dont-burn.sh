set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir

cancellerAddress=$(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr) \
signingKey=~/$BLOCKCHAIN_PREFIX/voter-0.skey
configurationUtxo=$(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

redeemer=$baseDir/redeemers/vote-validator/cancel.json
voteValidatorScript=$baseDir/vote-validator.plutus

voteUtxo=$( $baseDir/query/vote-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $cancellerAddress $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

upgradeProposalUtxo=$(./$baseDir/query/proposal-dummy-1.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)


cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $cancellerAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $cancellerAddress $BLOCKCHAIN) \
    --tx-in $voteUtxo \
    --tx-in-script-file $voteValidatorScript \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $redeemer \
    --read-only-tx-in-reference $configurationUtxo \
    --read-only-tx-in-reference $upgradeProposalUtxo \
    --tx-out "$cancellerAddress + 2137884 lovelace + 1 ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e.123456 $extraOutput" \
    --required-signer $signingKey \
    --change-address $cancellerAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $signingKey \
    $BLOCKCHAIN \
    --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile

echo "submitted transaction"

echo
