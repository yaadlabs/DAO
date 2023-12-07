# Repo-wide Nixpkgs with overlays
{ inputs, ... }:
{
  perSystem = { pkgs, system, ... }: {

    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      inherit (inputs.haskell-nix) config;
      overlays = [
        inputs.haskell-nix.overlay
        inputs.iohk-nix.overlays.crypto
      ];
    };

  };
}
