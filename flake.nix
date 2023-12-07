{
  description = "Triphut Dao";

  inputs = {
    lbf.url = "github:mlabs-haskell/lambda-buffers";
    haskell-nix.follows = "lbf/haskell-nix";
    pre-commit-hooks.follows = "lbf/pre-commit-hooks";
    nixpkgs.follows = "lbf/nixpkgs";
    iohk-nix.follows = "lbf/iohk-nix";
    flake-parts.follows = "lbf/flake-parts";
    plutarch.follows = "lbf/plutarch";
    psm.url = "github:mlabs-haskell/plutus-simple-model";
    plutonomy = {
      url = "github:well-typed/plutonomy";
      flake = false;
    };

  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/pkgs.nix
        ./nix/settings.nix
        ./nix/pre-commit.nix
        ./types/build.nix
        ./dao-lib/build.nix
      ];

      debug = true;

      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
