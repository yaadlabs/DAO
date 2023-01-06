set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir

treasuryLockerAddress=$1
signingKey=$2
output=$3
treasuryAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/treasury-validator.addr)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $treasuryLockerAddress $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $treasuryLockerAddress $BLOCKCHAIN) \
    --tx-out-inline-datum-value '[]' \
    --tx-out "$treasuryAddr + $output" \
    --required-signer $signingKey \
    --change-address $treasuryLockerAddress \
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
