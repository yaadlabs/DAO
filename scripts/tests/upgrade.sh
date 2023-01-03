set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh
$baseDir/generate-datums.sh

detected=false

"$baseDir/failure-cases/mint-invalid-configuration-nft-tx.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/lock-upgrade-proposal.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/dummy-tally-lock.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/failure-cases/upgrade-configuration-missing-proposal.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

detected=false

"$baseDir/failure-cases/upgrade-configuration-missing-tally.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

detected=false

"$baseDir/failure-cases/upgrade-configuration-wrong-upgrade-token.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

detected=false

"$baseDir/happy-path/upgrade-configuration.sh" || {
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

sleep 20

$baseDir/happy-path/upgrade-configuration.sh
