set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

deployerAddress=$1
signingKey=$2
datumFile=$3
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/configuration.addr)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $deployerAddress $BLOCKCHAIN)

currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$currentSlot
nextTenSlots=$(($currentSlot+45))

configurationMinterId=$(cat $baseDir/configuration-nft-policy-id.txt)
configurationMinterFile=$baseDir/configuration-nft.plutus

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

mintValue="1 $configurationMinterId.434F4E464947"

cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $deployerAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $deployerAddress $BLOCKCHAIN) \
    --tx-out "$scriptAddr + 2323090 lovelace + $mintValue" \
    --tx-out-inline-datum-file $datumFile \
    --tx-out "$deployerAddress + 2137884 lovelace $extraOutput" \
    --required-signer $signingKey \
    --change-address $deployerAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $configurationMinterFile \
    --mint-redeemer-value '[]' \
    --invalid-before $startSlot\
    --invalid-hereafter $nextTenSlots \
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
