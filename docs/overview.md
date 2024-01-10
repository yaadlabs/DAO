# Triphut DAO

While the `Readme` serves as an outline of the general idea behind the proposal system, this overview is intended to act as an evolving document of its component parts, in particular giving a more detailed view of the scripts that make up the project, with reference to their corresponding tests.

# Project structure

The project is structured in the following manner:

`dao/dao-specs` contains the unit tests written with the `plutus-simple-model` framework. It's an extensive test suite that covers all the smart contracts contained within the `dao/dao-lib` directory. The suite covers the expected 'happy path' for each branch of the contracts, ensuring that a valid transaction passes validation as expected, as well as negative tests that ensure that the validation checks in the contracts will catch and reject an illegal transaction.

`dao/dao-test` imports all the tests from the `dao/dao-specs` directory and runs them using the `tasty` testing library.

`dao/dao-lib` contains the core of the application, including the DAO validator and minting policy scripts, as well as utility functions shared across the modules. The scripts are contained in the following modules - `Dao.Configuration.Script`, `Dao.Vote.Script`, `Dao.Treasury.Script`, `Dao.Tally.Script`, `Dao.Index.Script`.

The application's types are contained in the `types` directory. The use of the `Lambda Buffers` library for representing these types allows us to circumvent some of the common issues, such as encoding errors, encountered when sharing types between the on-chain and the off-chain.

# Overview of the DAO's scripts

The scripts are made up of a number of minting policies and validators contained in the modules referenced above. We have added detailed haddock-style comments to each of the scripts contained within the `dao` directory, which we will also cover here.

## Dao.Configuration.Script

In the `Dao.Configuration.Script` module we have two scripts, a minting policy and a validator, which we will outline below.

The `Dao.Configuration.Script.mkConfigurationNftPolicy` is a one-shot minting policy script used for minting the NFT which marks the UTXO containing the `LambdaBuffers.ApplicationTypes.DynamicConfigDatum`. This configuration datum includes a number of fields for choosing how the application will function, we will reference it again in the section on the application's types below.

The `mkConfigurationNftPolicy` script performs the following validation checks:

  - The UTXO, referenced in the `ncInitialUtxo` field of
    the `NftConfig` argument, is spent in the transaction.
  - The token name matches the `ncTokenName` field of the `NftConfig` argument.
  - Exactly one configuration NFT is minted with the valid token name.
  - There is exactly one output containing the NFT.
  - This output contains a valid `LambdaBuffers.ApplicationTypes.DynamicConfigDatum` datum.

The tests for this policy can be found in the `Spec.Configuration.Context` module. 

The other script contained in this module is the validator script `Dao.Configuration.Script.validateConfiguration`. As you can see in the tests for the `mkConfigurationNftPolicy` policy script we pay the configuration NFT with the `DynamicConfigDatum` to the `validateConfiguration` script, which we refer to in the tests as the `upgradeConfigNftTypedValidavtor`, as this is the validator envoked when a transaction wishes to upgrade the configuration. This action can only occur when the type of the proposal is an `Upgrade` proposal.

The `validateConfiguration` script performs the following validation checks:

  - There is exactly one `LambdaBuffers.ApplicationTypes.Tally.TallyStateDatum` in the reference inputs,
    marked by the tally NFT (The corresponding tally `CurrencySymbol` is contained in the `tallyNft` field of the `DynamicConfigDatum`).
  - There is a configuration token in the inputs (we are spending the configuration input here not referencing it).
  - The proposal is an upgrade proposal (Dao.Types.ProposalType.Upgrade).
  - That one `Upgrade` token was minted in the transaction with the `CurrencySymbol` specified in the `ProposalType.Upgrade`
  - That the upgrade proposal has enough votes. We ensure this by checking that the number of votes recorded in the `TallyStateDatum` via the `for` and `against` fields are greater than or equal to the required majorities specified in the `upgradeRelativeMajorityPercent` and `upgradeMajorityPercent` fields of the `DynamicConfigDatum`. The `TallyStateDatum` must be included as a reference in the transaction.
  - That the time period for voting on the proposal has passed. We ensure this by checking the `proposalEndTime` (specified in the `TallyStateDatum`) added to the `proposalTallyEndOffset` against the validity range of the transaction, ensuring they sum to a time before the transaction's validity range.

The tests for this validator can be found in the `Spec.Upgrade.Context` module.

## Dao.Vote.Script

In the `Dao.Vote.Script` module we have two scripts, a minting policy and a validator which we will outline below.

The `Dao.Vote.Script.mkVoteMinter` script is used to check a transaction that wishes to mint or burn a vote token. The `VoteMinterActionRedeemer` redeemer is used to determine which action the transaction wishes to take, burning or minting.

The `mkVoteMinter` script performs the following validation checks:

When the `LambdaBuffers.ApplicationTypes.Vote.VoteMinterActionRedeemer` redeemer is set to `Mint`, this policy performs the following checks:

  - There is exactly one `LambdaBuffers.ApplicationTypes.Configuration.DynamicConfigDatum` in the reference inputs, marked by the configuration NFT (The corresponding `CurrencySymbol` and `TokenName` are provided by the `ConfigurationValidatorConfig` argument).
  - There is exactly one `Dao.Types.TallyStateDatum` in the reference inputs, marked by the Tally NFT.
  - Exactly one valid vote NFT is minted with the valid token name.
  - The token name matches the `voteTokenName` field of the `DynamicConfigDatum`.
  - There is exactly one output containing the vote NFT.
  - This output contains a valid `Dao.Vote.VoteDatum` datum.
  - The output token and valid datum are paid to the vote validator, specified in the `voteValidator` of the `DynamicConfigDatum` that must be included in the reference inputs.
  - The proposal is still active. We check this by ensuring the proposal end time provided by the `TallyStateDatum` is after the validity range of the transaction. The `TallyStateDatum` must be included in the reference inputs.
  - The total ADA is greater than the return ADA specified by the `returnAda` field of the `VoteDatum`.

When the `LambdaBuffers.ApplicationTypes.Vote.VoteMinterActionRedeemer` redeemer is set to `Burn`, this policy performs the following checks:

  - That one vote token is burned.

The tests for this policy script can be found in the `Spec.Vote.Context` module.

The `Dao.Vote.Script.validateVote` is used for validating a particular vote action which a transaction wants to execute. The `VoteActionRedeemer` redeemer is used to determine which action the transaction wishes to take, to `Count` or to `Cancel` the vote.

The `validateVote` script performs the following validation checks:

The validator always ensures the following, regardless of the redeemer provided:

  - There is exactly one `DynamicConfigDatum` in the reference inputs, marked by the config NFT. (The corresponding configuration `CurrencySymbol` and `TokenName` are provided by the `ConfigurationValidatorConfig` argument).

  - When the `VoteActionRedeemer` redeemer is set to `Count`, this validator performs the following checks:
  
    - That the tally validator is present in the inputs, the tally validator is specified by the `tallyValidator` field of the `DynamicConfigDatum` (included in the references), this is necessary as the two validators are used conjunction to ensure the validity of the transaction.

  - When the `VoteActionRedeemer` redeemer is set to `Cancel`, this validator performs the following checks:

    - The transaction is signed by the vote owner, specified by the `voteOwner` field of the `VoteDatum`.
    - All the vote tokens are burned, checking that there are no vote tokens in the transaction outputs, with the corresponding `CurrencySymbol` specified by the `voteCurrencySymbol` in the `DynamicConfigDatum`

The tests for both the vote validator and tally validator can be found in the `Spec.Vote.ContextValidator` module.

## Dao.Treasury.Script

In the `Dao.Treasury.Script` module we have one script, the treasury validator. The treasury validator checks vary based on the type of proposal, there are three types of proposals represented by the constructors of the `ProposalType` type, `Trip`, `General`, and `Upgrade` (Which we referenced earlier when discussing the `validateConfiguration` script).

This validator always ensures:

  - There is exactly one of this script contained in the transaction's inputs. This check is carried out using the `Dao.Treasury.Script.ownValueAndValidator` helper.

  - It uses the `proposal` field of `TallyStateDatum` like a redeemer, choosing which branch to follow based on the value of this field. (Trip, General, or Upgrade)

When the `proposal` field of `TallyStateDatum` is set to `Trip`, this validator performs the following checks:

  - The proposal has enough votes. The vote counts equal or exceed the values specified in the `tripRelativeMajorityPercent` and `tripMajorityPercent` fields of the `DynamicConfigDatum`.

  - The amount disbursed does not exceed the amount specified in the `maxTripDisbursement` field of the `DynamicConfigDatum`.

  - The correct amount is paid to the traveler's address, specified by the corresponding `Trip` constructor of the `ProposalType` type. The traveler's amount should be greater than or equal to the total cost of the travel minus the payment to the travel agent.

  - The correct amount is paid to the travel agent's address, specified by the corresponding `Trip` field in the `ProposalType`.

When the `proposal` field of `TallyStateDatum` is set to `General`, this validator performs the following checks:

  - The proposal has enough votes. The vote counts equal or exceed the values specified in the `generalRelativeMajorityPercent` and `generalMajorityPercent` fields of the `DynamicConfigDatum`.

  - The amount disbursed does not exceed the amount specified in the `maxGeneralDisbursement` field of the `DynamicConfigDatum`.

  - The correct amount is paid to the general payment address, specified by the corresponding `General` field in the `ProposalType`.

When the `proposal` field of `TallyStateDatum` is set to `Upgrade`, this validator performs the following checks:

  - The proposal has enough votes. The vote counts equal or exceed the values specified in the `upgradeRelativeMajorityPercent` and `upgradeMajorityPercent` fields of the `DynamicConfigDatum`.

  - That the proposal end time has passed. We do this by checking the sum of the `proposalEndtime` field of the `TallyStateDatum` and the `proposalTallyEndOffset` of the `DynamicConfigDatum` against the validity range of the transaction. Ensuring the sum of these values is less than lower part of the range.

  - That exactly one `upgradeMinter` token was minted. The `CurrencySymbol` for this token is provided as the field of the `Upgrade` constructor of the `ProposalType`.
  
  The tests for the treasury validator can be found in `Spec.Treasury.Context`.
  
## Dao.Tally.Script
  
In the `Dao.Tally.Script` module we have two scripts, the tallying minting policy and the tallying validator.

The `Dao.Tally.Script.mkTallyNftMinter` policy performs the following checks:

  - There is exactly one 'DynamicConfigDatum' in the reference inputs, marked by the config NFT (The corresponding configuration `CurrencySymbol` and `TokenName` provided by the `TallyNftConfig` argument).
  - There is exactly one Index UTXO spent (contained in the `txInfoInputs`).
  - This index UTXO contains a valid `IndexNftDatum` (The `Dao.Index.Script.validateIndex` validator ensures the datum's index is incremented by one).
  - Exactly one valid Tally NFT is minted with the valid token name.
  - The token name matches the `'ndex` field of the `IndexNftDatum`.
  - There is exactly one output containing the tally NFT.
  - This output contains a valid `TallyStateDatum` datum.
  - The initial votes for fields of the `TallyStateDatum` are both set to zero.
  - The tally output is at the tally validator (Corresponding to the tally script provided by the `tallyValidator` field of the `DynamicConfigDatum`).
  
The tests for the tally policy validator can be found in `Spec.Tally.Context`.

The `Dao.Tally.Script.validateTally` validator performs the following checks:

  - There is exactly one `DynamicConfigDatum` in the reference inputs, marked by the tally NFT. (The corresponding config `CurrencySymbol` and `TokenName` provided by the `ConfigurationValidatorConfig` argument).
  - That the tally NFT remains at the validator (the `newValueIsAtleastAsBigAsOldValue` check).
  - There is exactly one `TallyStateDatum` in the outputs.
  - This `TallyStateDatum` in the outputs has been updated accordingly. We check this by ensuring the new votes have been added to the `For` and `Against` vote count fields of the new tally datum at the output.
  - That the proposal period has passed. We do this by checking the `proposalEndTime` field of the `TallyStateDatum` against the transaction validity range, ensuring the proposal end time has passed.
  - That all vote tokens are burned (there are no vote tokens in the outputs).
  
As mentioned previously the tally validator is used in conjunction with the vote validator, so as a result the tests can be found in the same module `Spec.Vote.ContextValidator`.

## Dao.Tally.Index

In the `Dao.Index.Script` module we have two scripts, the tally index minting policy and the tallying validator.

The `Dao.Index.Script.mkIndexNftMinter` minting policy performs the following checks:

  - The UTXO, referenced in the `incInitialUtxo` field of the `IndexNftConfig` argument, is spent in the transaction.
  - The token name matches the `incTokenName` field of the `NftConfig` argument.
  - Exactly one valid config NFT is minted with the valid token name.
  - There is exactly one output containing the NFT.
  - This output contains a valid `IndexNftDatum` datum.
  - The `index` field of this datum is set to zero.
  - The index output is at the index validator (The corresponding index script provided by the `incIndexValidator` field of the `IndexNftConfig` parameter).

The tests for the index policy can be found in the `Spec.Index.Context` module.

The `Dao.Index.Script.indexValidator` script performs the following checks:

  - The `index` field of the `IndexNftDatum` is incremented when we create a new proposal (a new `TallyStateDatum` is created and paid to the tally validator).
  - The index NFT stays at the validator.

The index validator is used in conjunction with the tally minter, when minting a tally NFT the index validator is used to check that the index field of the `IndexNftDatum` was incremented by one, hence the index validator is also tested in the same place as the tally minter, `Spec.Tally.Context`.

## Types

All the application specific types can be found in the `types` directory. As mentioned previously the proposal type determines what type of proposal we are dealing with, a trip, a general or an upgrade proposal. The `TallyStateDatum` contains important information about the proposal - the proposal end time, the type of the proposal as well as how many votes are for and against the proposal. The `DynamicConfigDatum` can be found in the `Configuration` module, and contains all the application specific values, a number of which we referenced in our overview of the scripts. The `VoteDatum` is contained in the `Vote` module, and contains important information regarding the vote that was cast - such as the name of the proposal for which the vote was cast, the `direction` of the vote (for or against), and the owner of the vote (specified by an `Address`).
