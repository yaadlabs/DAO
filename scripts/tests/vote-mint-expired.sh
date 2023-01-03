set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh
$baseDir/generate-datums.sh

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/lock-upgrade-proposal.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

sleep 30

detected=false

"$baseDir/happy-path/mint-vote.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi
