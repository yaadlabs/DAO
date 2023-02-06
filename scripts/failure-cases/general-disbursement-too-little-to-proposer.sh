set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}


configurationUtxo=$(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
tallyUtxo=$(./$baseDir/query/tally-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
treasuryUtxo=$(./$baseDir/query/treasury-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/core/disburse-treasury.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tallyUtxo \
  $configurationUtxo \
  $treasuryUtxo \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr) \
  "3000000 lovelace" \
  "96000000 lovelace"
