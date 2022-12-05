set -eux
mkdir -p scripts/temp/
mkdir -p ~/$BLOCKCHAIN_PREFIX
./scripts/wallets/make-wallet-and-pkh.sh configuration-nft-deployer
./scripts/wallets/make-wallet-and-pkh.sh proposal-dummy
./scripts/wallets/make-wallet-and-pkh.sh voter-0
