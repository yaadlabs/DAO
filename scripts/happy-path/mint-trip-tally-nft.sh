set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

configurationUtxo=$(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/core/mint-tally-nft.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/index-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/index-nft-deployer.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/next-index.json \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/tally-travel-1.json \
  $configurationUtxo
