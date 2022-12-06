# Triphut DAO Smart Contracts

# Prerequistes

To run the tests and use the compile script, one must first install `cardano-cli-balance-fixer` from here: https://github.com/Canonical-LLC/cardano-cli-balance-fixer

Additionally, the `cardano-cli` must be installed and on the path.

# Creating Test Wallets

To compile for testing, we must create the test wallets first.

Run:

```bash
scripts/wallets/make-all-wallets.sh
```

# Environment Setup

Before building for testing, we must source the proper environment file. You depending on your specific setup you might have to modify the environment variables so the `CARDANO_NODE_SOCKET_PATH` points to correct location.

To use a local testnet for testing, source the following:

```bash
source scripts/envars/local-testnet.envars
```

# Building

To build install the GHC 8.10.7 and the cabal.

Next ensure that the

Then in the root directory call:

```bash
scripts/compile.sh INITIAL_CONFIGURATION_UTXO
```

Where `INITIAL_CONFIGURATION_UTXO` is the UTxO that will be used to create the configuration NFT.

Alternatively, for testing run

```bash
scripts/scratch/setup-wallets.sh
scripts/wait/until-next-block.sh
scripts/compile.sh
```

This will build all the code and compile the smart contracts.

The compiled contracts can be found in:
- scripts/configuration-nft.plutus
- scripts/configuration.plutus
- scripts/vote-minter.plutus
- scripts/vote-validator.plutus

# Testing

Before running the integration tests make sure the proper envars have been sourced and that the test wallets have Ada, by following the previous steps.

Then run:

```bash
scripts/tests/all.sh
```

## Example Transactions

The tests use example transactions utilizing the `cardano-cli`. They can be found in the directories `scripts/happy-path` and `scripts/failure-cases`
