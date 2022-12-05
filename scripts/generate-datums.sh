set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

prefix=${1:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

$thisDir/hash-plutus.sh


tallyIndexNft=$(cat $thisDir/configuration-hash.txt)
tallyNft=$(cat $thisDir/configuration-hash.txt)
tallyValidator=$(cat $thisDir/configuration-hash.txt)
treasuryValidator=$(cat $thisDir/configuration-hash.txt)
configurationValidator=$(cat $thisDir/configuration-hash.txt)
voteCurrencySymbol=$(cat $thisDir/vote-minter-policy-id.txt)
voteTokenName=564F5445
voteValidator=$(cat $thisDir/vote-validator-hash.txt)
updateMajoriyPercent=500 # 50 percent
upgradeRelativeMajorityPercent=100 # ten percent
totalVotes=10
voteNft=d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2
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
    }
  ]
}

EOF


$thisDir/hash-datums.sh
