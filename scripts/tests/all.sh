set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$thisDir/upgrade.sh
$thisDir/vote-mint-and-cancel.sh
$thisDir/vote-mint-and-count.sh
