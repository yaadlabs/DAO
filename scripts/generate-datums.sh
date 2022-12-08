set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

prefix=${1:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

$thisDir/hash-plutus.sh

offset=${2:-10000}

nowSeconds=$(date +%s)
now=$(($nowSeconds*1000))
endTime=$(($nowSeconds*1000+$offset))
superLongEndTime=$(($nowSeconds*1000+10000000))


tallyIndexNft=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e
tallyNft=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e
tallyTokenName=54414C4C59
tallyValidator=$(cat $thisDir/always-succeed-hash.txt)
treasuryValidator=$(cat $thisDir/always-succeed-hash.txt)
configurationValidator=$(cat $thisDir/configuration-hash.txt)
voteCurrencySymbol=$(cat $thisDir/vote-minter-policy-id.txt)
voteTokenName=564F5445
voteValidator=$(cat $thisDir/vote-validator-hash.txt)
updateMajoriyPercent=500 # 50 percent
upgradeRelativeMajorityPercent=110 # ten percent
totalVotes=10
voteNft=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e
voteFungibleCurrencySymbol=a37767c537bbf908aa2bf5abf49ef3fd67e749cbca3225d31bd166e0
voteFungibleTokenName=544F4B454E

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/initial-configuration.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes" : "$tallyIndexNft"
    },
    {
      "bytes" : "$tallyNft"
    },
    {
      "bytes" : "$tallyTokenName"
    },
    {
      "bytes" : "$tallyValidator"
    },
    {
      "bytes" : "$treasuryValidator"
    },
    {
      "bytes" : "$configurationValidator"
    },
    {
      "bytes" : "$voteCurrencySymbol"
    },
    {
      "bytes" : "$voteTokenName"
    },
    {
      "bytes" : "$voteValidator"
    },
    {
      "int" : $updateMajoriyPercent
    },
    {
      "int" : $upgradeRelativeMajorityPercent
    },
    {
      "int" : $totalVotes
    },
    {
      "bytes": "$voteNft"
    },
    {
      "bytes": "$voteFungibleCurrencySymbol"
    },
    {
      "bytes": "$voteFungibleTokenName"
    },
    {
      "int": $offset
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/invalid-configuration.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes" : "$tallyIndexNft"
    },
    {
      "bytes" : "$tallyNft"
    },
    {
      "bytes" : "$tallyValidator"
    },
    {
      "bytes" : "$treasuryValidator"
    },
    {
      "bytes" : "$configurationValidator"
    },
    {
      "bytes" : "$voteCurrencySymbol"
    },
    {
      "bytes" : "$voteTokenName"
    },
    {
      "bytes" : "$voteValidator"
    },
    {
      "int" : $updateMajoriyPercent
    },
    {
      "int" : $upgradeRelativeMajorityPercent
    },
    {
      "int" : $totalVotes
    },
    {
      "bytes": "$voteNft"
    },
    {
      "bytes": "$voteFungibleCurrencySymbol"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/always-succeed-upgrade-proposal.json
{
  "constructor": 0,
  "fields": [
    {
      "int": $endTime
    },
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e"
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/always-succeed-upgrade-proposal-1.json
{
  "constructor": 0,
  "fields": [
    {
      "int": $superLongEndTime
    },
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e"
        }
      ]
    }
  ]
}

EOF


$thisDir/hash-datums.sh
