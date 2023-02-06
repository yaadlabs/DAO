set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

minterAddress=$1
signingKey=$2
datumFile=$3
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/index-validator.addr)

redeemer=$baseDir/redeemers/vote-minter/mint.json

bodyFile=$tempDir/vote-mint-tx-body.01
outFile=$tempDir/vote-mint-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $minterAddress $BLOCKCHAIN)


indexNftId=$(cat $baseDir/tally-index-policy-id.txt)
voteMinterFile=$baseDir/tally-index.plutus
mintValue="1 $indexNftId.494e444558"

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi


currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
nextTenSlots=$(($currentSlot+60))


cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $minterAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $minterAddress $BLOCKCHAIN) \
    --tx-out "$scriptAddr + 3500000 lovelace + $mintValue" \
    --tx-out-inline-datum-file $datumFile \
    --tx-out "$minterAddress + 2137884 lovelace $extraOutput" \
    --required-signer $signingKey \
    --change-address $minterAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $voteMinterFile \
    --mint-redeemer-file $redeemer \
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
