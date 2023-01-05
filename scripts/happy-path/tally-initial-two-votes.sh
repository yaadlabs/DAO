set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
tempDir=$baseDir/../temp

export DATUM_PREFIX=${DATUM_PREFIX:-0}

voteUtxo0=$(./$baseDir/query/vote-validator.sh | grep 3500000 | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
voteUtxo1=$(./$baseDir/query/vote-validator.sh | grep 3600000 | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)
tallyUtxo=$(./$baseDir/query/tally-validator.sh | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)

voteNftPolicyId=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e
voteNft0=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e.30
voteNft1=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e.31

tallyNftPolicyId=$(cat $baseDir/tally-nft-policy-id.txt)
tallyNft=$tallyNftPolicyId.30

$baseDir/core/update-tally.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.addr) \
  ~/$BLOCKCHAIN_PREFIX/configuration-nft-deployer.skey \
  $tallyUtxo \
  "2000000 lovelace + 1 $tallyNft" \
  $tempDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/tally-1.json \
  $voteUtxo0 \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-0.addr) \
  "1400000 lovelace + 1 $voteNft0" \
  $voteUtxo1 \
  $(cat ~/$BLOCKCHAIN_PREFIX/voter-1.addr) \
  "1400000 lovelace + 1 $voteNft1"
