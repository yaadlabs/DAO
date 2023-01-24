set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh 22
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
$baseDir/happy-path/mint-trip-tally-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-vote0-long-lived.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-vote1-long-lived.sh
$baseDir/wait/until-next-block.sh
sleep 20
$baseDir/happy-path/tally-trip-initial-two-votes.sh
$baseDir/wait/until-next-block.sh

sleep 80
detected=false

"$baseDir/failure-cases/trip-disbursement-too-little-back.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent too little back"
  exit 1
fi

detected=false
"$baseDir/failure-cases/trip-disbursement-too-little-to-agent.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent too little to agent"
  exit 1
fi

detected=false
"$baseDir/failure-cases/trip-disbursement-too-little-to-traveler.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent too little to traveler"
  exit 1
fi

$baseDir/happy-path/trip-disbursement.sh
$baseDir/wait/until-next-block.sh

echo "Success"
