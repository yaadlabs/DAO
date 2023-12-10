{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.dao-lb-types = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "dao-lb-types";
        src = ./.;
        files =
          [ "ApplicationTypes/Proposal.lbf"
            "ApplicationTypes/Vote.lbf"
            "ApplicationTypes/Configuration.lbf"
            "ApplicationTypes/Index.lbf"
            "ApplicationTypes/Tally.lbf"
          ];
      };
    };
}
