set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh 24
$baseDir/generate-datums.sh

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/lock-treasury.sh
$baseDir/wait/until-next-block.sh

$baseDir/minting/mint-vote-0-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-vote-0-fungible.sh
$baseDir/wait/until-next-block.sh
$baseDir/minting/mint-vote-1-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-tally-index-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-tally-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-vote0-fungible.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-vote1-fungible.sh
$baseDir/wait/until-next-block.sh
sleep 20
$baseDir/happy-path/tally-initial-two-votes-fungible.sh
$baseDir/wait/until-next-block.sh

sleep 80
$baseDir/happy-path/upgrade-configuration.sh
$baseDir/wait/until-next-block.sh
