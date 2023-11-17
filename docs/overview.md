# Triphut DAO

While the `Readme` serves as an outline of the general idea behind the proposal system, this overview is intended to act as an evolving document of its component parts, in particular giving a more detailed view of the scripts that make up the project, with reference to their corresponding tests.

# Project structure

The project is structured in the following manner:

`triphut-specs` contains the unit tests written with the `plutus-simple-model` framework. It's an extensive test suite that covers all the smart contracts contained within the `triphut` directory. The suite covers the expected 'happy path' for each branch of the contracts, ensuring that a valid transaction passes validation as expected, as well as negative tests that ensure that the validation checks in the contracts will catch and reject an illegal transaction.

`triphut-test` imports all the tests from the `triphut-specs` directory and runs them using the `tasty` testing library. 

`triphut` contains the core of the application, including the application's types, the DAO validator and minting policy scripts, as well as utility functions shared across the modules.

Within the `triphut` directory we have a number of outer modules which represent the logical division of the application, these modules include - `Triphut.Types`, `Triphut.ConfigurationNft`, `Triphut.Vote`, `Triphut.Treasury`, `Triphut.Tally`, `Triphut.Index`, which contain the application's types - as well as the `Triphut.Shared` module which contains a number of utility functions for use across the other modules. 

Then nested a step below we have the corresponding scripts under - `Triphut.ConfigurationNft.Script`, `Triphut.Vote.Script`, `Triphut.Treasury.Script`, `Triphut.Tally.Script`, `Triphut.Index.Script`.

# Overview of the DAO's scripts

The scripts are made up of a number of minting policies and validators contained in the modules referenced above. We have added detailed haddock-style comments to each of the scripts contained within the `triphut` directory, which we will also cover here.

## Triphut.ConfigurationNft.Script

In the `Triphut.ConfigurationNft.Script` module we have two scripts, a minting policy and a validator, which we will outline below.

The `Triphut.ConfigurationNft.Script.mkConfigurationNftPolicy` is a one-shot minting policy script used for minting the NFT which marks the UTXO containing the `Triphut.Types.DynamicConfigDatum`. This configuration datum includes a number of fields for choosing how the application will function, we will reference it again in the section on the application's types below.

The `mkConfigurationNftPolicy` script performs the following validation checks:

  - The UTXO, referenced in the `ncInitialUtxo` field of
    the `NftConfig` argument, is spent in the transaction.
  - The token name matches the `ncTokenName` field of the `NftConfig` argument.
  - Exactly one configuration NFT is minted with the valid token name.
  - There is exactly one output containing the NFT.
  - This output contains a valid `Triphut.Types.DynamicConfigDatum` datum.

The tests for this policy can be found in the `Spec.ConfigurationNft.Context` module. 

The other script contained in this module is the validator script `Triphut.ConfigurationNft.Script.validateConfiguration`. As you can see in the tests for the `mkConfigurationNftPolicy` policy script we pay the configuration NFT with the `DynamicConfigDatum` to the `validateConfiguration` script, which we refer to in the tests as the `upgradeConfigNftTypedValidavtor`, as this is the validator envoked when a transaction wishes to upgrade the configuration. This action can only occur when the type of the proposal is an `Upgrade` proposal.

The `validateConfiguration` script performs the following validation checks:

  - There is exactly one `Triphut.Tally.TallyStateDatum` in the reference inputs,
    marked by the tally NFT (The corresponding tally `CurrencySymbol` is contained in the `dcTallyNft` field of the `DynamicConfigDatum`).
  - There is a configuration token in the inputs (we are spending the configuration input here not referencing it).
  - The proposal is an upgrade proposal (Triphut.Types.ProposalType.Upgrade).
  - That one `Upgrade` token was minted in the transaction with the `CurrencySymbol` specified in the `ProposalType.Upgrade`
  - That the upgrade proposal has enough votes. We ensure this by checking that the number of votes recorded in the `TallyStateDatum` via the `tsFor` and `tsAgainst` fields are greater than or equal to the required majorities specified in the `dcUpgradeRelativeMajorityPercent` and `dcUpgradeMajorityPercent` fields of the `DynamicConfigDatum`. The `TallyStateDatum` must be included as a reference in the transaction.
  - That the time period for voting on the proposal has passed. We ensure this by checking the `tsProposalEndTime` (specified in the `TallyStateDatum`) added to the `dcProposalTallyEndOffset` against the validity range of the transaction, ensuring they sum to a time before the transaction's validity range.

The tests for this validator can be found in the `Spec.Upgrade.Context` module.

## Triphut.Vote.Script

In the `Triphut.Vote.Script` module we have two scripts, a minting policy and a validator which we will outline below.

The `Triphut.Vote.Script.mkVoteMinter` script is used to check a transaction that wishes to mint or burn a vote token. The `VoteMinterActionRedeemer` redeemer is used to determine which action the transaction wishes to take, burning or minting.

The `mkVoteMinter` script performs the following validation checks:

When the `Triphut.Vote.VoteMinterActionRedeemer` redeemer is set to `Mint`, this policy performs the following checks:

  - There is exactly one `Triphut.Vote.VoteMinterDynamicConfigDatum` in the reference inputs, marked by the configuration NFT (The corresponding `CurrencySymbol` and `TokenName` are provided by the `VoteMinterConfig` argument).
  - There is exactly one `Triphut.Types.TallyStateDatum` in the reference inputs, marked by the Tally NFT.
  - Exactly one valid vote NFT is minted with the valid token name.
  - The token name matches the `vmdcVoteTokenName` field of the `VoteMinterDynamicConfigDatum`.
  - There is exactly one output containing the vote NFT.
  - This output contains a valid `Triphut.Vote.VoteDatum` datum.
  - The output token and valid datum are paid to the vote validator, specified in the `vmdcVoteValidator` of the `VoteMinterDynamicConfigDatum` that must be included in the reference inputs.
  - The proposal is still active. We check this by ensuring the proposal end time provided by the `TallyStateDatum` is after the validity range of the transaction. The `TallyStateDatum` must be included in the reference inputs.
  - The total ADA is greater than the return ADA specified by the `vReturnAda` field of the `VoteDatum`.

When the `Triphut.Vote.VoteMinterActionRedeemer` redeemer is set to `Burn`, this policy performs the following checks:

  - That one vote token is burned.

The tests for this policy script can be found in the `Spec.Vote.Context` module.

The `Triphut.Vote.Script.validateVote` is used for validating a particular vote action which a transaction wants to execute. The `VoteActionRedeemer` redeemer is used to determine which action the transaction wishes to take, to `Count` or to `Cancel` the vote.

The `validateVote` script performs the following validation checks:

The validator always ensures the following, regardless of the redeemer provided:

  - There is exactly one `Triphut.Vote.VoteDynamicConfigDatum` in the reference inputs, marked by the config NFT. (The corresponding configuration `CurrencySymbol` and `TokenName` are provided by the `VoteValidatorConfig` argument).

  - When the `Triphut.Vote.VoteActionRedeemer` redeemer is set to `Count`, this validator performs the following checks:
  
    - That the tally validator is present in the inputs, the tally validator is specified by the `vdcTallyValidator` field of the `VoteDynamicConfigDatum` (included in the references), this is necessary as the two validators are used conjunction to ensure the validity of the transaction.

  - When the `Triphut.Vote.VoteActionRedeemer` redeemer is set to `Cancel`, this validator performs the following checks:

    - The transaction is signed by the vote owner, specified by the `vOwner` field of the `Triphut.Vote.VoteDatum`.
    - All the vote tokens are burned, checking that there are no vote tokens in the transaction outputs, with the corresponding `CurrencySymbol` specified by the `vdcVoteCurrencySymbol` in the `VoteDynamicConfigDatum`

The tests for both the vote validator and tally validator can be found in the `Spec.Vote.ContextValidator` module.

## Triphut.Treasury.Script

In the `Triphut.Treasury.Script` module we have one script, the treasury validator. The treasury validator checks vary based on the type of proposal, there are three types of proposals represented by the constructors of the `Triphut.Types.ProposalType` type, `Trip`, `General`, and `Upgrade` (Which we referenced earlier when discussing the `validateConfiguration` script).

This validator always ensures:

  - There is exactly one of this script contained in the transaction's inputs. This check is carried out using the `Triphut.Treasury.Script.ownValueAndValidator` helper.

  - It uses the `tsProposal` field of `Triphut.Types.TallyStateDatum` like a redeemer, choosing which branch to follow based on the value of this field. (Trip, General, or Upgrade)

When the `tsProposal` field of `Triphut.Types.TallyStateDatum` is set to `Trip`, this validator performs the following checks:

  - The proposal has enough votes. The vote counts equal or exceed the values specified in the `dcTripRelativeMajorityPercent` and `dcTripMajorityPercent` fields of the `Triphut.Types.DynamicConfigDatum`.

  - The amount disbursed does not exceed the amount specified in the `dcMaxTripDisbursement` field of the `Triphut.Types.DynamicConfigDatum`.

  - The correct amount is paid to the traveler's address, specified by the corresponding `Trip` constructor of the `ProposalType` type. The traveler's amount should be greater than or equal to the total cost of the travel minus the payment to the travel agent.

  - The correct amount is paid to the travel agent's address, specified by the corresponding `Trip` field in the `ProposalType`.

When the `tsProposal` field of `Triphut.Types.TallyStateDatum` is set to `General`, this validator performs the following checks:

  - The proposal has enough votes. The vote counts equal or exceed the values specified in the `dcGeneralRelativeMajorityPercent` and `dcGeneralMajorityPercent` fields of the `Triphut.Types.DynamicConfigDatum`.

  - The amount disbursed does not exceed the amount specified in the `dcMaxGeneralDisbursement` field of the `Triphut.Types.DynamicConfigDatum`.

  - The correct amount is paid to the general payment address, specified by the corresponding `General` field in the `ProposalType`.

When the `tsProposal` field of `Triphut.Types.TallyStateDatum` is set to `Upgrade`, this validator performs the following checks:

  - The proposal has enough votes. The vote counts equal or exceed the values specified in the `dcUpgradeRelativeMajorityPercent` and `dcUpgradeMajorityPercent` fields of the `Triphut.Types.DynamicConfigDatum`.

  - That the proposal end time has passed. We do this by checking the sum of the `tsProposalEndtime` field of the `Triphut.Types.TallyStateDatum` and the `dcProposalTallyEndOffset` of the `Triphut.Types.DynamicConfigDatum` against the validity range of the transaction. Ensuring the sum of these values is less than lower part of the range.

  - That exactly one `upgradeMinter` token was minted. The `CurrencySymbol` for this token is provided as the field of the `Upgrade` constructor of the `ProposalType`.
  
  The tests for the treasury validator can be found in `Spec.Treasury.Context`.
  
## Triphut.Tally.Script
  
In the `Triphut.Tally.Script` module we have two scripts, the tallying minting policy and the tallying validator.

The `Triphut.Tally.Script.mkTallyNftMinter` policy performs the following checks:

  - There is exactly one 'DynamicConfigDatum' in the reference inputs, marked by the config NFT (The corresponding configuration `CurrencySymbol` and `TokenName` provided by the `TallyNftConfig` argument).
  - There is exactly one Index UTXO spent (contained in the `txInfoInputs`).
  - This index UTXO contains a valid `IndexNftDatum` (The `Triphut.Index.Script.validateIndex` validator ensures the datum's index is incremented by one).
  - Exactly one valid Tally NFT is minted with the valid token name.
  - The token name matches the `indIndex` field of the `IndexNftDatum`.
  - There is exactly one output containing the tally NFT.
  - This output contains a valid `Triphut.Types.TallyStateDatum` datum.
  - The initial votes for fields of the `Triphut.Types.TallyStateDatum` are both set to zero.
  - The tally output is at the tally validator (Corresponding to the tally script provided by the `dcTallyValidator` field of the `Triphut.Types.DynamicConfigDatum`).
  
The tests for the tally policy validator can be found in `Spec.Tally.Context`.

The `Triphut.Tally.Script.validateTally` validator performs the following checks:

  - There is exactly one `Triphut.Tally.TallyDynamicConfigDatum` in the reference inputs, marked by the tally NFT. (The corresponding config `CurrencySymbol` and `TokenName` provided by the `TallyValidatorConfig` argument).
  - That the tally NFT remains at the validator (the `newValueIsAtleastAsBigAsOldValue` check).
  - There is exactly one `Triphut.Tally.TallyStateDatum` in the outputs.
  - This `Triphut.Tally.TallyStateDatum` in the outputs has been updated accordingly. We check this by ensuring the new votes have been added to the `tsFor` and `tsAgainst` vote count fields of the new tally datum at the output.
  - That the proposal period has passed. We do this by checking the `tsProposalEndTime` field of the `TallyStateDatum` against the transaction validity range, ensuring the proposal end time has passed.
  - That all vote tokens are burned (there are no vote tokens in the outputs).
  
As mentioned previously the tally validator is used in conjunction with the vote validator, so as a result the tests can be found in the same module `Spec.Vote.ContextValidator`.

## Triphut.Tally.Index

In the `Triphut.Index.Script` module we have two scripts, the tally index minting policy and the tallying validator.

The `Triphut.Index.Script.mkIndexNftMinter` minting policy performs the following checks:

  - The UTXO, referenced in the `incInitialUtxo` field of the `IndexNftConfig` argument, is spent in the transaction.
  - The token name matches the `incTokenName` field of the `NftConfig` argument.
  - Exactly one valid config NFT is minted with the valid token name.
  - There is exactly one output containing the NFT.
  - This output contains a valid `Triphut.Index.IndexNftDatum` datum.
  - The `index` field of this datum is set to zero.
  - The index output is at the index validator (The corresponding index script provided by the `incIndexValidator` field of the `IndexNftConfig` parameter).

The tests for the index policy can be found in the `Spec.Index.Context` module.

The `Triphut.Index.Script.indexValidator` script performs the following checks:

  - The `index` field of the `Triphut.Index.IndexNftDatum` is incremented when we create a new proposal (a new `TallyStateDatum` is created and paid to the tally validator).
  - The index NFT stays at the validator.

The index validator is used in conjunction with the tally minter, when minting a tally NFT the index validator is used to check that the index field of the `IndexNftDatum` was incremented by one, hence the index validator is also tested in the same place as the tally minter, `Spec.Tally.Context`.

## Types

The `ProposalType` and `TallyStateDatum` can both be found in the `Triphut.Types` module, as mentioned previously the proposal type determines what type of proposal we are dealing with, a trip, a general or an upgrade proposal. The `TallyStateDatum` contains important information about the proposal - the proposal end time, the type of the proposal as well as how many votes are for and against the proposal. The other upper-level `triphut` modules we mentioned earlier contain the types specific to their function, for example the `DynamicConfigDatum` can be found in the `Triphut.ConfigurationNft` module, and contains all the application specific values, a number of which we referenced in our overview of the scripts. The `VoteDatum` is contained in the `Triphut.Vote` module, and contains important information regarding the vote that was cast - such as the name of the proposal for which the vote was cast, the `direction` of the vote (for or against), and the owner of the vote (specified by an `Address`).