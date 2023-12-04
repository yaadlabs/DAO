{
  description = "Triphut DAO";

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    plutarch.url = "github:plutonomicon/plutarch-plutus";
    lbf.url = "github:mlabs-haskell/lambda-buffers";
  };

  outputs = inputs@{ self, tooling, plutarch, nixpkgs, ... }:
    tooling.lib.mkFlake { inherit self; }
      {
        imports = [
          (tooling.lib.mkHaskellFlakeModule1 {
            project.src = ./.;
            project.extraHackage = [
              "${inputs.lbf.packages.x86_64-linux.lbf-plutus-haskell}"
              "${inputs.lbf.packages.x86_64-linux.lbr-plutus-haskell-src}"

              "${inputs.lbf.packages.x86_64-linux.lbf-prelude-haskell}"
              "${inputs.lbf.packages.x86_64-linux.lbr-prelude-haskell-src}"
            ];
          })
        ];
      };
}
