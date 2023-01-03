set -eux



thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir
$baseDir/hash-plutus.sh
$baseDir/hash-datums.sh

voterAddress=$1
signingKey=$2
datumFile=$3
proposalUtxo=$4
configurationUtxo=$5
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/vote-validator.addr)

redeemer=$baseDir/redeemers/vote-minter/mint.json

bodyFile=$tempDir/vote-mint-tx-body.01
outFile=$tempDir/vote-mint-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $voterAddress $BLOCKCHAIN -o '1 ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e.123456')


voteMinterId=$(cat $baseDir/vote-minter-policy-id.txt)
voteMinterFile=$baseDir/vote-minter.plutus
mintValue="1 $voteMinterId.564F5445"

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
    $(cardano-cli-balance-fixer input --address $voterAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $voterAddress $BLOCKCHAIN) \
    --read-only-tx-in-reference $configurationUtxo \
    --read-only-tx-in-reference $proposalUtxo \
    --tx-out "$scriptAddr + 3500000 lovelace + 1 ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e.123456 + $mintValue" \
    --tx-out-inline-datum-file $datumFile \
    --tx-out "$voterAddress + 2137884 lovelace $extraOutput" \
    --required-signer $signingKey \
    --change-address $voterAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $voteMinterFile \
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
