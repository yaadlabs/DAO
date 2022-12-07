set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir

updaterAddress=$1
signingKey=$2
tallyUtxo=$3
proposalUtxo=$4
upgradeMinterId=$5
upgradeTokenName=$6
upgradeScript=$7

configurationValidatorScript=$baseDir/configuration.plutus

configurationNft=$(cat $baseDir/configuration-nft-policy-id.txt)

configurationUtxo=$( $baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $updaterAddress $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi


mintValue="1 $upgradeMinterId.$upgradeTokenName"

currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
nextTenSlots=$(($currentSlot+15))

cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $updaterAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $updaterAddress $BLOCKCHAIN) \
    --tx-in $configurationUtxo \
    --tx-in-script-file $configurationValidatorScript \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-value '[]' \
    --read-only-tx-in-reference $tallyUtxo \
    --read-only-tx-in-reference $proposalUtxo \
    --tx-out "$updaterAddress + 2137884 lovelace + $mintValue + 1 $configurationNft.434F4E464947 $extraOutput" \
    --required-signer $signingKey \
    --change-address $updaterAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $upgradeScript \
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
