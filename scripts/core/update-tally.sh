set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir

updaterAddress=$1
signingKey=$2
tallyUtxo=$3
tallyOutput=$4
newTallyDatum=$5
voteUtxo0=$6
voteOwner0=$7
voteOutput0=$8
voteUtxo1=$9
voteOwner1=${10}
voteOutput1=${11}


voteScript=$baseDir/vote-validator.plutus
tallyScript=$baseDir/tally-validator.plutus
tallyScriptAddr=$(cat $baseDir/$BLOCKCHAIN_PREFIX/tally-validator.addr)

configurationUtxo=$( $baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $updaterAddress $BLOCKCHAIN)

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
nextTenSlots=$(($currentSlot+20))

voteRedeemer=$baseDir/redeemers/vote-validator/count.json

voteMinterId=$(cat $baseDir/vote-minter-policy-id.txt)
voteMinterFile=$baseDir/vote-minter.plutus
mintValue="-2 $voteMinterId.564F5445"
mintRedeemer=$baseDir/redeemers/vote-minter/burn.json

cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $updaterAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $updaterAddress $BLOCKCHAIN) \
    --tx-in $tallyUtxo \
    --tx-in-script-file $tallyScript \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-value '[]' \
    --tx-in $voteUtxo0 \
    --tx-in-script-file $voteScript \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $voteRedeemer \
    --tx-in $voteUtxo1 \
    --tx-in-script-file $voteScript \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file $voteRedeemer \
    --read-only-tx-in-reference $configurationUtxo \
    --tx-out "$voteOwner0 + $voteOutput0" \
    --tx-out "$voteOwner1 + $voteOutput1" \
    --tx-out "$tallyScriptAddr + $tallyOutput" \
    --tx-out-inline-datum-file $newTallyDatum \
    --required-signer $signingKey \
    --change-address $updaterAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $voteMinterFile \
    --mint-redeemer-file $mintRedeemer \
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
