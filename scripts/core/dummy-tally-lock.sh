set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

mkdir -p $tempDir

tallyAddress=$1
signingKey=$2
datumFile=$3
scriptAddr=$4
tallyNftPolicyId=$5
tallyNftTokenName=$6
tallyNftMintScript=$7


bodyFile=$tempDir/sell-tx-body.01
outFile=$tempDir/sell-tx.01
changeOutput=$(cardano-cli-balance-fixer change --address $tallyAddress $BLOCKCHAIN)


extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi

mintValue="1 $tallyNftPolicyId.$tallyNftTokenName"

cardano-cli transaction build \
    --babbage-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $tallyAddress $BLOCKCHAIN) \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $tallyAddress $BLOCKCHAIN) \
    --tx-out "$scriptAddr + 2823090 lovelace + $mintValue" \
    --tx-out-inline-datum-file $datumFile \
    --tx-out "$tallyAddress + 2137884 lovelace $extraOutput" \
    --required-signer $signingKey \
    --change-address $tallyAddress \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --mint "$mintValue" \
    --mint-script-file $tallyNftMintScript \
    --mint-redeemer-value [] \
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
