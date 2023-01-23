set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../



$thisDir/vote-mint-and-cancel.sh
$thisDir/vote-mint-and-count-upgrade.sh
$thisDir/vote-mint-and-count-general.sh
$thisDir/vote-mint-expired.sh
$thisDir/upgrade-no-relative-majority.sh
$thisDir/upgrade-too-few-votes.sh
