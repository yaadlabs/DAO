set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..
tempDir=$mainDir/temp

initialConfigurationUtxo=$(./scripts/query/configuration-nft-deployer.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)

(
cd $mainDir
cabal run exe:create-sc -- \
  --always-succeed-output=scripts/always-succeed.plutus \
  --always-succeed-hash-output=scripts/always-succeed-hash.txt \
  --always-succeed1-output=scripts/always-succeed-1.plutus \
  --always-succeed1-hash-output=scripts/always-succeed-1-hash.txt \
  --configuration-nft-output=scripts/configuration-nft.plutus \
  --configuration-nft-policy-id-output=scripts/configuration-nft-policy-id.txt \
  --configuration-nft-token-name=CONFIG \
  --configuration-nft-initial-utxo=$initialConfigurationUtxo \
  --configuration-validator-output=scripts/configuration.plutus \
  --configuration-validator-hash-output=scripts/configuration-hash.txt \
  --vote-minter-output=scripts/vote-minter.plutus \
  --vote-minter-policy-id-output=scripts/vote-minter-policy-id.txt \
  --vote-validator-output=scripts/vote-validator.plutus \
  --vote-validator-hash-output=scripts/vote-validator-hash.txt
)

$thisDir/hash-plutus.sh
