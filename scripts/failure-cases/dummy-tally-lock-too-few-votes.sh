set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

upgradeProposalUtxo=$(./$baseDir/query/proposal-dummy.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

upgradeProposalTxId=$( echo $upgradeProposalUtxo | cut -d '#' -f 1)
upgradeProposalIndex=$( echo $upgradeProposalUtxo | cut -d '#' -f 2)

$baseDir/generate-tally-datums.sh $DATUM_PREFIX $upgradeProposalTxId $upgradeProposalIndex

$baseDir/core/dummy-tally-lock.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/final-upgrade-tally-too-few-votes.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/always-succeed.addr) \
  ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e \
  54414C4C59 \
  $baseDir/test-policies/test-policy-0.plutus \
  2900000
