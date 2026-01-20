{
  description = "Triphut Dao";

  inputs = {
    lbf.url = "github:mlabs-haskell/lambda-buffers/v1.0.0";
    flake-lang.follows = "lbf/flake-lang";
    haskell-nix.follows = "flake-lang/haskell-nix";
    pre-commit-hooks.follows = "flake-lang/pre-commit-hooks";
    nixpkgs.follows = "flake-lang/nixpkgs";
    iohk-nix.follows = "flake-lang/iohk-nix";
    flake-parts.follows = "flake-lang/flake-parts";
    plutarch.follows = "flake-lang/plutarch";
    # PSM is deprecated
    # psm.url = "github:mlabs-haskell/plutus-simple-model";
    # plutonomy = {
    #   url = "github:well-typed/plutonomy";
    #   flake = false;
    # };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/pkgs.nix
        ./nix/settings.nix
        ./nix/pre-commit.nix
        ./types/build.nix
        ./dao/build.nix
      ];

      debug = true;

      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
