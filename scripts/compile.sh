set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..
tempDir=$mainDir/temp

initialConfigurationUtxo=$(./scripts/query/configuration-nft-deployer.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)

(
cd $mainDir
cabal run exe:create-sc -- \
  --configuration-nft-output=scripts/configuration-nft.plutus \
  --configuration-nft-policy-id-output=scripts/configuration-nft-policy-id.txt \
  --configuration-nft-token-name=CONFIG \
  --configuration-nft-initial-utxo=$initialConfigurationUtxo \
  --configuration-validator-output=scripts/configuration.plutus \
  --configuration-validator-hash-output=scripts/configuration-hash.txt
)

$thisDir/hash-plutus.sh
