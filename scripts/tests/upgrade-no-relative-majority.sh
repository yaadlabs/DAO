set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh 123
$baseDir/generate-datums.sh

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/lock-treasury.sh
$baseDir/wait/until-next-block.sh

$baseDir/minting/mint-vote-0-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-vote-1-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-tally-index-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-tally-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-vote0-long-lived.sh
$baseDir/wait/until-next-block.sh
sleep 20
$baseDir/happy-path/tally-initial-one-vote.sh
$baseDir/wait/until-next-block.sh

sleep 80

detected=false

"$baseDir/failure-cases/upgrade-configuration-no-relative-majority.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent no relative majority"
  exit 1
fi

echo "Success"
