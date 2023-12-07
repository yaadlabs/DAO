{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.dao-lb-types = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "dao-lb-types";
        src = ./.;
        files = [ "Types/ApplicationTypes.lbf" ];
      };
    };
}
