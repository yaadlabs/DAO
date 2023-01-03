set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=/Users/jonathanfischoff/prototypes/cardano-node/example/utxo-keys/utxo1.skey
senderAddr=$(cardano-cli address build --testnet-magic "42" --payment-verification-key-file /Users/jonathanfischoff/prototypes/cardano-node/example/utxo-keys/utxo1.vkey)
outFile=temp/consolidate-tx.01
configurationNftDeployer=$(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr)
voter0=$(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr)
voter1=$(cat ~/$BLOCKCHAIN_PREFIX/voter-1.addr)

cardano-cli transaction build \
  --babbage-era \
  $BLOCKCHAIN \
  $(cardano-cli-balance-fixer input --address $senderAddr $BLOCKCHAIN ) \
  --tx-out "$configurationNftDeployer + 100000000 lovelace" \
  --tx-out "$voter0 + 100000000 lovelace" \
  --tx-out "$voter1 + 100000000 lovelace" \
  --change-address $senderAddr \
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
