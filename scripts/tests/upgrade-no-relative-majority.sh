set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh
$baseDir/generate-datums.sh


$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/lock-upgrade-proposal.sh
$baseDir/wait/until-next-block.sh
$baseDir/failure-cases/dummy-tally-lock-no-relative-majority.sh
$baseDir/wait/until-next-block.sh

sleep 30

detected=false

"$baseDir/failure-cases/upgrade-configuration-no-relative-majority.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent no relative majority"
  exit 1
fi
