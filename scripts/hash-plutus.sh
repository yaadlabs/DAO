set -eux
thisDir=$(dirname "$0")

cardano-cli address build \
 --payment-script-file $thisDir/configuration.plutus \
 --mainnet \
 --out-file $thisDir/mainnet/configuration.addr

cardano-cli address build \
 --payment-script-file $thisDir/configuration.plutus \
 --testnet-magic 1097911063 \
 --out-file $thisDir/testnet/configuration.addr

 cardano-cli address build \
 --payment-script-file $thisDir/configuration.plutus \
 --testnet-magic 42 \
 --out-file $thisDir/local-testnet/configuration.addr

cardano-cli address build \
 --payment-script-file $thisDir/vote-validator.plutus \
 --mainnet \
 --out-file $thisDir/mainnet/vote-validator.addr

cardano-cli address build \
 --payment-script-file $thisDir/vote-validator.plutus \
 --testnet-magic 1097911063 \
 --out-file $thisDir/testnet/vote-validator.addr

 cardano-cli address build \
 --payment-script-file $thisDir/vote-validator.plutus \
 --testnet-magic 42 \
 --out-file $thisDir/local-testnet/vote-validator.addr

cardano-cli address build \
 --payment-script-file $thisDir/always-succeed.plutus \
 --mainnet \
 --out-file $thisDir/mainnet/always-succeed.addr

cardano-cli address build \
 --payment-script-file $thisDir/always-succeed.plutus \
 --testnet-magic 1097911063 \
 --out-file $thisDir/testnet/always-succeed.addr

 cardano-cli address build \
 --payment-script-file $thisDir/always-succeed.plutus \
 --testnet-magic 42 \
 --out-file $thisDir/local-testnet/always-succeed.addr

cardano-cli address build \
 --payment-script-file $thisDir/always-succeed-1.plutus \
 --mainnet \
 --out-file $thisDir/mainnet/always-succeed-1.addr

cardano-cli address build \
 --payment-script-file $thisDir/always-succeed-1.plutus \
 --testnet-magic 1097911063 \
 --out-file $thisDir/testnet/always-succeed-1.addr

 cardano-cli address build \
 --payment-script-file $thisDir/always-succeed-1.plutus \
 --testnet-magic 42 \
 --out-file $thisDir/local-testnet/always-succeed-1.addr
