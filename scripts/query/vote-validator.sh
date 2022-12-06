# This is just a scratch for me to remember this commmand
thisDir=$(dirname "$0")
baseDir=$thisDir/../
cardano-cli query utxo --address $(cat $baseDir/$BLOCKCHAIN_PREFIX/vote-validator.addr) $BLOCKCHAIN
