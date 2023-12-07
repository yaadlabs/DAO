{ inputs, ... }:
{
  perSystem = { config, system, inputs', ... }:
    let
      hsFlake = inputs.lbf.lib.${system}.haskellPlutusFlake {
        src = ./.;

        name = "dao";

        inherit (config.settings.haskell) index-state compiler-nix-name;

        dependencies = [
          # PSM
          "${inputs.psm}/cardano-simple"
          "${inputs.psm}/psm"
          "${inputs.plutarch}/plutarch-extra"
          "${inputs.plutarch}"

          # Plutonomy
          "${inputs.plutonomy}"

          # Lambda Buffers types
          "${config.packages.dao-lb-types}"

          # LBs PlutusTx
          "${inputs'.lbf.packages.lbf-plutus-haskell}"
          "${inputs'.lbf.packages.lbr-plutus-haskell-src}"

          # LBs Prelude
          "${inputs'.lbf.packages.lbf-prelude-haskell}"
          "${inputs'.lbf.packages.lbr-prelude-haskell-src}"
        ];

        devShellTools = config.settings.shell.tools;
        devShellHook = config.settings.shell.hook;
      };

    in
    {
      devShells.dao-lib = hsFlake.devShell;

      packages = {
        dao-lib = hsFlake.packages."dao:lib:dao";

        dao-app = hsFlake.packages."dao:exe:create-sc";
      };

      inherit (hsFlake) checks;
    };
}
