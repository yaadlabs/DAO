set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/mint-tally-index-nft.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/index-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/index-nft-deployer.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/initial-index.json
