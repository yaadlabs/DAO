set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/compile.sh 12
$baseDir/generate-datums.sh

$baseDir/happy-path/mint-configuration-nft-tx.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/lock-treasury.sh
$baseDir/wait/until-next-block.sh

$baseDir/happy-path/mint-tally-index-nft.sh
$baseDir/wait/until-next-block.sh
$baseDir/happy-path/mint-tally-nft.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/happy-path/mint-vote0-long-lived.sh" || {
    echo "Failed to prevent minting without vote token!"
    detected=true
}

if [ $detected == false ]; then
  exit 1
fi

$baseDir/minting/mint-vote-0-nft.sh
$baseDir/wait/until-next-block.sh

detected=false

"$baseDir/failure-cases/mint-vote-bad-output-address.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent witness escaping!"
  exit 1
fi

detected=false

"$baseDir/failure-cases/mint-vote-counted.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent counted vote from minting!"
  exit 1
fi

"$baseDir/failure-cases/mint-vote-too-little-ada.sh" || {
    detected=true
}

if [ $detected == false ]; then
  echo "Failed to prevent too little ada vote from minting!"
  exit 1
fi

$baseDir/happy-path/mint-vote0-long-lived.sh
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
$baseDir/wait/until-next-block.sh
