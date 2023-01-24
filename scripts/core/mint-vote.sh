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
tallyUtxo=$4
configurationUtxo=$5
voteNft=$6
lovelaceAmount=$7
scriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/vote-validator.addr)

redeemer=$baseDir/redeemers/vote-minter/mint.json

bodyFile=$tempDir/vote-mint-tx-body.01
outFile=$tempDir/vote-mint-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $voterAddress $BLOCKCHAIN -o "1 $voteNft")


voteMinterId=$(cat $baseDir/vote-minter-policy-id.txt)
voteMinterFile=$baseDir/vote-minter.plutus
mintValue="1 $voteMinterId.564F5445"


currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
nextTenSlots=$(($currentSlot+60))


cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $voterAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $voterAddress $BLOCKCHAIN) \
    --read-only-tx-in-reference $configurationUtxo \
    --read-only-tx-in-reference $tallyUtxo \
    --tx-out "$scriptAddr + $lovelaceAmount lovelace + 1 $voteNft + $mintValue" \
    --tx-out-inline-datum-file $datumFile \
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
