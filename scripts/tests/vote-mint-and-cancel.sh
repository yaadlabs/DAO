set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh
$baseDir/generate-datums.sh

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/lock-long-lived-upgrade-proposal.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-vote.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/cancel-vote.sh
