set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh
$baseDir/generate-datums.sh

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/lock-long-lived-upgrade-proposal.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/happy-path/mint-vote.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/mint-vote.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/failure-cases/cancel-vote-wrong-signer.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

detected=false

"$baseDir/failure-cases/cancel-vote-dont-burn.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

detected=false

"$baseDir/failure-cases/cancel-missing-config-nft.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/happy-path/cancel-vote.sh