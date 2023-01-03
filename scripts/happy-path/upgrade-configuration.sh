set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

tallyUtxo=$(./$baseDir/query/tally-validator.sh | grep 3000000 | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
proposalUtxo=$(./$baseDir/query/proposal-dummy.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/core/upgrade-configuration.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tallyUtxo \
  $proposalUtxo \
  ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e \
  33 \
  $baseDir/test-policies/test-policy-0.plutus
