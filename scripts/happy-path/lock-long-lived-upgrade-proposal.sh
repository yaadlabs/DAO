set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/lock-upgrade-proposal.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/always-succeed-upgrade-proposal-1.json \
  $(cat ~/$BLOCKCHAIN_PREFIX/proposal-dummy-1.addr)
