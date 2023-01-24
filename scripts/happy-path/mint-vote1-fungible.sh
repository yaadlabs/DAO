set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

configurationUtxo=$(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
tallyUtxo=$(./$baseDir/query/tally-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/core/mint-vote.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-1.addr) \
  ~/$BLOCKCHAIN_PREFIX/voter-1.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/vote-1-fungible.json \
  $tallyUtxo \
  $configurationUtxo \
  "ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e.31" \
  3600000
