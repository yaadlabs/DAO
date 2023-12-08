{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.dao-lb-types = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "dao-lb-types";
        src = ./.;
        files =
          [ "Types/ApplicationTypes.lbf"
            "Types/Vote.lbf"
            "Types/Configuration.lbf"
            "Types/Index.lbf"
            "Types/Tally.lbf"
          ];
      };
    };
}
