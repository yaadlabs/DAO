set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

endTime=$(jq '.fields[1].int' $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/start.json)

$baseDir/core/bid-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  $(cat $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-bid-1-hash.txt) \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/escrow-bid-1.json \
  "10000000 lovelace" \
  ~/$BLOCKCHAIN_PREFIX/marketplace.skey
