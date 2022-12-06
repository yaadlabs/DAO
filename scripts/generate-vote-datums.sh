set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

prefix=$1
txOutRefTxId=$2
txOutRefIndex=$3

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

$thisDir/hash-plutus.sh

voter0Pkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/voter-0-pkh.txt)

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/vote-0.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields" : [
        {
          "constructor": 0,
          "fields" : [
              {
                "bytes" : "$txOutRefTxId"
              }
            ]
        },
        {
          "int": $txOutRefIndex
        }
      ]
    },
    {
      "constructor": 0,
      "fields" : []
    },
    {
      "constructor": 0,
      "fields" : []
    },
    { "constructor": 0,
      "fields": [
        { "constructor": 0,
          "fields": [
            {
              "bytes": "$voter0Pkh"
            }
          ]
        },
        {
          "constructor": 1,
          "fields": []
        }
      ]
    },
    {
      "int": 3000000
    }
  ]
}

EOF

$thisDir/hash-datums.sh
