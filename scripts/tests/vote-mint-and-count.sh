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
$baseDir/happy-path/mint-vote-long-lived.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/dummy-tally-long-lived-lock.sh
$baseDir/wait/until-next-block.sh

$baseDir/failure-cases/bad-dummy-tally-long-lived-lock.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/failure-cases/count-vote-bad-tally-utxo.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/happy-path/count-vote.sh
