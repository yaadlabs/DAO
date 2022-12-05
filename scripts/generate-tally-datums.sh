set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

prefix=$1
txOutRefTxId=$2
txOutRefIndex=$3

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

$thisDir/hash-plutus.sh

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/final-upgrade-tally.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields" : [
        {
          "bytes" : "$txOutRefTxId"
        },
        {
          "int": $txOutRefIndex
        }
      ]
    },
    {
      "int" : 3
    },
    {
      "int" : 2
    }
  ]
}

EOF

$thisDir/hash-datums.sh
