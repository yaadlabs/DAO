set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

tallyUtxo=$(./$baseDir/query/tally-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
proposalUtxo=$(./$baseDir/query/proposal-dummy.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/core/upgrade-configuration.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tallyUtxo \
  $proposalUtxo \
  7038197ba9c25791cf7849d3727c812075f07d29cb4f049eab741400 \
  33 \
  $baseDir/test-policies/test-policy-1.plutus
