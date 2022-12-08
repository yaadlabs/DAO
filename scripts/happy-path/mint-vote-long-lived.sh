set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

upgradeProposalUtxo=$(./$baseDir/query/proposal-dummy-1.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

upgradeProposalTxId=$( echo $upgradeProposalUtxo | cut -d '#' -f 1)
upgradeProposalIndex=$( echo $upgradeProposalUtxo | cut -d '#' -f 2)

configurationUtxo=$(./$baseDir/query/configuration-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

$baseDir/generate-vote-datums.sh $DATUM_PREFIX $upgradeProposalTxId $upgradeProposalIndex

$baseDir/core/mint-vote.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr) \
  ~/$BLOCKCHAIN_PREFIX/voter-0.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/vote-0.json \
  $upgradeProposalUtxo \
  $configurationUtxo
