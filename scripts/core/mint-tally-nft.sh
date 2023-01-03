set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

minterAddress=$1
signingKey=$2
indexDatumFile=$3
tallyDatumFile=$4
configurationUtxo=$5
indexScriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/index-validator.addr)
tallyScriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/tally-validator.addr)

redeemer=$baseDir/redeemers/vote-minter/mint.json

bodyFile=$tempDir/tally-nft-mint-tx-body.01
outFile=$tempDir/tally-nft-mint-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $minterAddress $BLOCKCHAIN)

indexUtxo=$( $baseDir/query/index-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

indexValidatorScriptFile=$baseDir/index-validator.plutus

tallyNftId=$(cat $baseDir/tally-nft-policy-id.txt)
tallyMinterFile=$baseDir/tally-nft.plutus
mintValue="1 $tallyNftId.30"

indexNftId=$(cat $baseDir/tally-index-policy-id.txt)
indexNft="1 $indexNftId.494e444558"

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
    --tx-in $indexUtxo \
    --tx-in-script-file $indexValidatorScriptFile \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-value '[]' \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $minterAddress $BLOCKCHAIN) \
    --read-only-tx-in-reference $configurationUtxo \
    --tx-out "$indexScriptAddr + 3500000 lovelace + $indexNft" \
    --tx-out-inline-datum-file $indexDatumFile \
    --tx-out "$tallyScriptAddr + 3500000 lovelace + $mintValue" \
    --tx-out-inline-datum-file $tallyDatumFile \
    --tx-out "$minterAddress + 2137884 lovelace $extraOutput" \
    --required-signer $signingKey \
    --change-address $minterAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $tallyMinterFile \
    --mint-redeemer-file $redeemer \
    --invalid-before $startSlot \
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
