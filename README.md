# Triphut DAO Smart Contracts

# Testing

There is a full test suite contained in the `triphut-specs` directory, written with the `plutus-simple-model` framework. The suite covers all the scripts contained within the `triphut` directory.

To run the test suite use the following command:
```
cabal test
```

# Project overview

You can find an overview of the project and the scripts contained within it in the `docs/overview.md` file.

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
- scripts/index-validator.plutus
- scripts/tally-index.plutus
- scripts/tally-nft.plutus
- scripts/tally-validator.plutus
- scripts/treasury-validator.plutus
- scripts/vote-minter.plutus
- scripts/vote-validator.plutus

Along with their hashes and policy ids

- scripts/configuration-hash.txt
- scripts/configuration-nft-policy-id.txt
- scripts/index-validator-hash.txt
- scripts/tally-index-policy-id.txt
- scripts/tally-nft-policy-id.txt
- scripts/tally-validator-hash.txt
- scripts/treasury-validator-hash.txt
- scripts/vote-minter-policy-id.txt
- scripts/vote-validator-hash.txt

# Testing

Before running the integration tests make sure the proper envars have been sourced and that the test wallets have Ada, by following the previous steps.

Then run:

```bash
scripts/tests/all.sh
```

## Example Transactions

The tests use example transactions utilizing the `cardano-cli`. They can be found in the directories `scripts/happy-path` and `scripts/failure-cases`

# Design

## Configuration

There is a single configuration NFT, that signifies which UTxO datum to use for configuration. All smart contracts are compiled against this NFT policy id and token name, and expect the NFT UTxO to be added to transaction as a reference input.

The configuration includes data such as the percent needed for a relative majority, as well as the other validator hashes and policy ids of the rest of the smart contract system.

### Modifying the Configuration

The configuration can be modified by upgrading the system. See **Upgrade Proposals** for more details.

## Voting

The DAO is designed to be decentralized and still allow voting without contention. To solve the contention problem of updating vote counts for a proposal, voting and tallying are split out as two separate processes.

Voting occurs first, and a vote minter contract is used to ensure that the vote is valid. Specifically it checks that:

1. The proposal is still active
2. The vote NFT is used for voting.
3. The user has included enough Ada.
4. The UTxO is outputted to the vote validator.
5. The vote is not counted.

If the vote is valid, a UTxO is created with the a vote datum that stores the proposal and direction of the vote (e.g. for or against). Additionally, a witness token is created that checked during tallying, to verify the vote occured at the correct time.

Users have to include min Ada for the their vote NFT, and additionally ada to cover the fees of tallying their vote. Users can cancel their votes at any time and receive all the assets the used to create the vote UTxO.

# Proposals

There are are three types of proposals the DAO can vote on, **General**, **Travel**, and **Upgrade** proposals.

## General Proposals

General proposals allow an individual to receive up to the max amount of Ada as configured by the `dcMaxGeneralDisbursement` setting, in the global configuration datum. They use the following constructor:

```haskell
General
  { ptGeneralPaymentAddress :: Address
  , ptGeneralPaymentValue   :: Integer
  }
```

They require the relative majority and majority percentage configured by `dcGeneralRelativeMajorityPercent` and `dcGeneralMajorityPercent` respectively.

## Trip Proposals

Trip proposals allow an individual to receive up to the max amount of Ada as configured by the `dcMaxTripDisbursement` setting, in the global configuration datum. They use the following constructor:

```haskell
Trip
  { ptTravelAgentAddress :: Address
  , ptTravelerAddress    :: Address
  , ptTotalTravelCost    :: Integer
  }
```

They require the relative majority and majority percentage configured by `dcTripRelativeMajorityPercent` and `dcTripMajorityPercent` respectively.

## Upgrade Proposals

The smart contract system is designed to be upgraded. Upgrades are special proposals that can be voted on. The proposal includes a minting contract that is used to validate the upgrade transaction. The security of the upgrade is delegated to this contract. The upgrade procedure ensures that there are enough votes for the upgrade, and that an upgrade token is minted in the transaction. This ensures that the upgrade minting contract is executed, so therefore the upgrade is valid.

The upgrade proposal has the following type:

```haskell
Upgrade
  { ptUpgradeMinter :: CurrencySymbol
  }
```
