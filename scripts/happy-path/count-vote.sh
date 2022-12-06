set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}


$baseDir/core/count-vote.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr) \
  ~/$BLOCKCHAIN_PREFIX/voter-0.skey \
  $(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo) \
  $(./$baseDir/query/tally-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo) \
  $baseDir/always-succeed.plutus \
  $baseDir/redeemers/vote-validator/count.json
