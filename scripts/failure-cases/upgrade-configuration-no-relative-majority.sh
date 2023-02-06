set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}


configurationUtxo=$(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
tallyUtxo=$(./$baseDir/query/tally-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
treasuryUtxo=$(./$baseDir/query/treasury-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/core/upgrade-configuration.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tallyUtxo \
  $configurationUtxo \
  ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e \
  33 \
  $baseDir/test-policies/test-policy-0.plutus \
  $treasuryUtxo
