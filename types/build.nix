{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.dao-lb-types = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "dao-lb-types";
        src = ./.;
        files =
          [
            "ApplicationTypes/Proposal.lbf"
            "ApplicationTypes/Vote.lbf"
            "ApplicationTypes/Configuration.lbf"
            "ApplicationTypes/Index.lbf"
            "ApplicationTypes/Tally.lbf"
            "ApplicationTypes/Arguments.lbf"
          ];
      };

      packages.dao-lb-types-purescript = inputs.lbf.lib."${system}".lbfPlutusPurescript {
        name = "dao-lb-types-purescript";
        src = ./.;
        files =
          [
            "ApplicationTypes/Proposal.lbf"
            "ApplicationTypes/Vote.lbf"
            "ApplicationTypes/Configuration.lbf"
            "ApplicationTypes/Index.lbf"
            "ApplicationTypes/Tally.lbf"
            "ApplicationTypes/Arguments.lbf"
          ];
      };

      packages.dao-lb-config = inputs.lbf.lib."${system}".lbfPreludeHaskell {
        name = "dao-lb-config";
        src = ./.;
        files = [ "ApplicationConfig/Scripts.lbf" ];
      };

      packages.dao-lb-config-purescript = inputs.lbf.lib."${system}".lbfPreludePurescript {
        name = "dao-lb-config-purescript";
        src = ./.;
        files = [ "ApplicationConfig/Scripts.lbf" ];
      };

    };
}
