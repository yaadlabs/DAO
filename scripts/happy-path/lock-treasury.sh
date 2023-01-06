set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/lock-treasury.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/treasury-locker.addr) \
  ~/$BLOCKCHAIN_PREFIX/treasury-locker.skey \
  "100000000 lovelace"
