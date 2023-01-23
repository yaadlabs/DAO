set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

prefix=${1:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

$thisDir/hash-plutus.sh

offset=${2:-40000}

nowSeconds=$(date +%s)
now=$(($nowSeconds*1000))
endTime=$(($nowSeconds*1000+$offset))
superLongEndTime=$(($nowSeconds*1000+10000000))

voter0Pkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/voter-0-pkh.txt)
voter1Pkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/voter-1-pkh.txt)

tallyIndexNft=ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e
tallyNft=$(cat $thisDir/tally-nft-policy-id.txt)
tallyTokenName=54414C4C59
tallyValidator=$(cat $thisDir/tally-validator-hash.txt)
treasuryValidator=$(cat $thisDir/treasury-validator-hash.txt)
configurationValidator=$(cat $thisDir/configuration-hash.txt)
voteCurrencySymbol=$(cat $thisDir/vote-minter-policy-id.txt)
voteTokenName=564F5445
voteValidator=$(cat $thisDir/vote-validator-hash.txt)
updateMajoriyPercent=500 # 50 percent
upgradeRelativeMajorityPercent=110 # eleven percent
genericMajoriyPercent=500 # 50 percent
genericRelativeMajorityPercent=110 # eleven percent
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
      "int" : $genericMajoriyPercent
    },
    {
      "int" : $genericRelativeMajorityPercent
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
      "int": $((offset*2))
    },
    {
      "int": 6000000
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
      "int" : $genericMajoriyPercent
    },
    {
      "int" : $genericRelativeMajorityPercent
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
      "int": 6000000
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


cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/initial-index.json
{
  "constructor": 0,
  "fields": [
    {
      "int": 0
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/next-index.json
{
  "constructor": 0,
  "fields": [
    {
      "int": 1
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/tally-1.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e"
        }
      ]
    },
    {
      "int": $endTime
    },
    {
      "int": 0
    },
    {
      "int": 0
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/tally-one-vote.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e"
        }
      ]
    },
    {
      "int": $endTime
    },
    {
      "int": 1
    },
    {
      "int": 0
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/tally-2.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "ce8822885d18e7d304ef0248af49359d687a94f0e3635eea14c6154e"
        }
      ]
    },
    {
      "int": $endTime
    },
    {
      "int": 2
    },
    {
      "int": 0
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/tally-general-1.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 1,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "constructor": 0,
              "fields" : [
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
          "int": 4000000
        }
      ]
    },
    {
      "int": $endTime
    },
    {
      "int": 0
    },
    {
      "int": 0
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/tally-general-2.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 1,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "constructor": 0,
              "fields" : [
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
          "int": 4000000
        }
      ]
    },
    {
      "int": $endTime
    },
    {
      "int": 2
    },
    {
      "int": 0
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/vote-0.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "30"
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

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/vote-1.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "30"
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
              "bytes": "$voter1Pkh"
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
