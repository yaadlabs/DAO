set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}


$baseDir/core/cancel-vote.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-1.addr) \
  ~/$BLOCKCHAIN_PREFIX/voter-1.skey \
  $(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo) \
  $(./$baseDir/query/proposal-dummy-1.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
