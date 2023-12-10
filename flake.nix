{
  description = "Triphut DAO";

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";

    plutarch.url = "github:plutonomicon/plutarch-plutus";
    lbf.url = "github:mlabs-haskell/lambda-buffers";
    # psm.url = "github:mlabs-haskell/plutus-simple-model";
    psm.url = "/home/nini/dev/triphut-repos/plutus-simple-model?rev=f97756a00a90197faf39ccfd144bbb7f8bd9f16d";
    
    plutonomy = {
      url = "github:well-typed/plutonomy";
      flake = false;
    };

  };

  outputs = inputs@{ self, tooling, ... }:
    tooling.lib.mkFlake { inherit self; }
      {
        imports = [
          (tooling.lib.mkHaskellFlakeModule1 {
            project.src = ./.;
            project.extraHackage = [
              "${inputs.plutarch}"
              "${inputs.plutarch}/plutarch-extra"

              "${inputs.plutonomy}"

              "${inputs.lbf.packages.x86_64-linux.lbf-plutus-haskell}"
              "${inputs.lbf.packages.x86_64-linux.lbr-plutus-haskell-src}"

              "${inputs.lbf.packages.x86_64-linux.lbf-prelude-haskell}"
              "${inputs.lbf.packages.x86_64-linux.lbr-prelude-haskell-src}"

              "${inputs.psm}/cardano-simple"
              "${inputs.psm}/psm"
            ];
          })
        ];
        
      };

}
