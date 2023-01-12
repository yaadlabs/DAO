set -eux
thisDir=$(dirname "$0")
mainDir=$thisDir/..
tempDir=$mainDir/temp

indexValidatorNonce=${1:- 0}

initialConfigurationUtxo=${2:-$(./scripts/query/configuration-nft-deployer.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)}
initialIndexUtxo=${3:-$(./scripts/query/index-nft-deployer.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)}

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
  --vote-validator-hash-output=scripts/vote-validator-hash.txt \
  --treasury-validator-output=scripts/treasury-validator.plutus \
  --treasury-validator-hash-output=scripts/treasury-validator-hash.txt \
  --tally-index-nft-output=scripts/tally-index.plutus \
  --tally-index-nft-policy-id-output=scripts/tally-index-policy-id.txt \
  --tally-index-nft-token-name=INDEX \
  --tally-index-nft-initial-utxo=$initialIndexUtxo \
  --tally-nft-output=scripts/tally-nft.plutus \
  --tally-nft-policy-id-output=scripts/tally-nft-policy-id.txt \
  --index-validator-output=scripts/index-validator.plutus \
  --index-validator-hash-output=scripts/index-validator-hash.txt \
  --tally-validator-output=scripts/tally-validator.plutus \
  --tally-validator-hash-output=scripts/tally-validator-hash.txt \
  --index-validator-nonce=$indexValidatorNonce
)

$thisDir/hash-plutus.sh
