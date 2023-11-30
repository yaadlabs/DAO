{
  description = "Triphut DAO";

  inputs = {
    nixpkgs-upstream.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    haskell-nix-extra-hackage = {
      url = "github:mlabs-haskell/haskell-nix-extra-hackage";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-nix.follows = "haskell-nix";
    };

    psm = {
      url = "github:mlabs-haskell/plutus-simple-model/31a18e5dc28ae7620c06adfad061f06ec176346b";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plutonomy = {
      url = "github:well-typed/plutonomy";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };

    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };

    plutus = {
      url = "github:input-output-hk/plutus/a56c96598b4b25c9e28215214d25189331087244";
      flake = false;
    };

    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/cb9eba406ceb2df338d8384b35c8addfe2067201";
      flake = false;
    };

    cardano-addresses = {
      url = "github:input-output-hk/cardano-addresses/b7273a5d3c21f1a003595ebf1e1f79c28cd72513";
      flake = false;
    };

    hw-aeson = {
      url = "github:sevanspowell/hw-aeson/b5ef03a7d7443fcd6217ed88c335f0c411a05408";
      flake = false;
    };

    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
      flake = false;
    };

    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };

    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };

    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/c7c63dabdb215ebdaed8b63274965966f2bf408f";
      flake = false;
    };

    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };

    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
      flake = false;
    };

    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/ea6d78c775d0f70dde979b52de022db749a2cc32";
      flake = false;
    };

    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };

    io-sim = {
      url =
        "github:input-output-hk/io-sim/57e888b1894829056cb00b7b5785fdf6a74c3271";
      flake = false;
    };

    typed-protocols = {
      url =
        "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
      flake = false;
    };

    ekg-forward = {
      url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };

    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };

    flat = {
      url =
        "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };

    cardano-wallet = {
      url = "github:input-output-hk/cardano-wallet/18a931648550246695c790578d4a55ee2f10463e";
      flake = false;
    };

    freer-extras = {
      url = "github:mlabs-haskell/freer-extras";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-upstream
    , haskell-nix
    , haskell-nix-extra-hackage
    , iohk-nix
    , pre-commit-hooks
    , ...
    }:
    let
      plainNixpkgsFor = system: import nixpkgs-upstream { inherit system; };
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays =
          [
            haskell-nix.overlay
            (iohk-nix.overlays.crypto)
          ];
      };

      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs-upstream.lib.genAttrs supportedSystems;

      fourmoluFor = system: (plainNixpkgsFor system).haskellPackages.fourmolu;
      hlintFor = system: (plainNixpkgsFor system).haskellPackages.hlint_3_4_1;

      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;

            settings = {
              ormolu.defaultExtensions = [
                "ImportQualifiedPost"
              ];
            };

            hooks = {
              cabal-fmt.enable = false;
              fourmolu.enable = false;
              hlint.enable = false;
              markdownlint.enable = false;
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              statix.enable = true;
              stylish-haskell.enable = true;
            };

            tools = {
              fourmolu = fourmoluFor system;
              hlint = hlintFor system;
            };
          };

      compiler-nix-name = "ghc8107";

      hackagesFor = system: haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
        [
          "${inputs.psm}"
          "${inputs.plutonomy}"

          "${inputs.plutus}/plutus-core"
          "${inputs.plutus}/plutus-ledger-api"
          "${inputs.plutus}/plutus-tx"
          "${inputs.plutus}/plutus-tx-plugin"
          "${inputs.plutus}/prettyprinter-configurable"
          "${inputs.plutus}/stubs/plutus-ghc-stub"
          "${inputs.plutus}/word-array"

          "${inputs.ouroboros-network}/monoidal-synchronisation"
          "${inputs.ouroboros-network}/network-mux"
          "${inputs.ouroboros-network}/ntp-client"
          "${inputs.ouroboros-network}/ouroboros-consensus"
          "${inputs.ouroboros-network}/ouroboros-consensus-byron"
          "${inputs.ouroboros-network}/ouroboros-consensus-cardano"
          "${inputs.ouroboros-network}/ouroboros-consensus-protocol"
          "${inputs.ouroboros-network}/ouroboros-consensus-shelley"
          "${inputs.ouroboros-network}/ouroboros-network"
          "${inputs.ouroboros-network}/ouroboros-network-framework"
          "${inputs.ouroboros-network}/ouroboros-network-testing"

          "${inputs.cardano-base}/base-deriving-via"
          "${inputs.cardano-base}/binary"
          "${inputs.cardano-base}/binary/test"
          "${inputs.cardano-base}/cardano-crypto-class"
          "${inputs.cardano-base}/cardano-crypto-praos"
          "${inputs.cardano-base}/cardano-crypto-tests"
          "${inputs.cardano-base}/measures"
          "${inputs.cardano-base}/orphans-deriving-via"
          "${inputs.cardano-base}/slotting"
          "${inputs.cardano-base}/strict-containers"

          "${inputs.cardano-prelude}/cardano-prelude"
          "${inputs.cardano-prelude}/cardano-prelude-test"

          "${inputs.cardano-crypto}"

          "${inputs.cardano-ledger}/eras/alonzo/impl"
          "${inputs.cardano-ledger}/eras/alonzo/test-suite"
          "${inputs.cardano-ledger}/eras/babbage/impl"
          "${inputs.cardano-ledger}/eras/byron/chain/executable-spec"
          "${inputs.cardano-ledger}/eras/byron/crypto"
          "${inputs.cardano-ledger}/eras/byron/crypto/test"
          "${inputs.cardano-ledger}/eras/byron/ledger/executable-spec"
          "${inputs.cardano-ledger}/eras/byron/ledger/impl"
          "${inputs.cardano-ledger}/eras/byron/ledger/impl/test"
          "${inputs.cardano-ledger}/eras/shelley-ma/impl"
          "${inputs.cardano-ledger}/eras/shelley-ma/test-suite"
          "${inputs.cardano-ledger}/eras/shelley/impl"
          "${inputs.cardano-ledger}/eras/shelley/test-suite"
          "${inputs.cardano-ledger}/libs/cardano-data"
          "${inputs.cardano-ledger}/libs/cardano-ledger-core"
          "${inputs.cardano-ledger}/libs/cardano-ledger-pretty"
          "${inputs.cardano-ledger}/libs/cardano-protocol-tpraos"
          "${inputs.cardano-ledger}/libs/non-integral"
          "${inputs.cardano-ledger}/libs/set-algebra"
          "${inputs.cardano-ledger}/libs/small-steps"
          "${inputs.cardano-ledger}/libs/small-steps-test"
          "${inputs.cardano-ledger}/libs/vector-map"

          "${inputs.cardano-wallet}/lib/cli"
          "${inputs.cardano-wallet}/lib/core"
          "${inputs.cardano-wallet}/lib/core-integration"
          "${inputs.cardano-wallet}/lib/dbvar"
          "${inputs.cardano-wallet}/lib/launcher"
          "${inputs.cardano-wallet}/lib/numeric"
          "${inputs.cardano-wallet}/lib/shelley"
          "${inputs.cardano-wallet}/lib/strict-non-empty-containers"
          "${inputs.cardano-wallet}/lib/test-utils"
          "${inputs.cardano-wallet}/lib/text-class"

          "${inputs.cardano-addresses}/command-line"
          "${inputs.cardano-addresses}/core"

          "${inputs.Win32-network}"

          "${inputs.iohk-monitoring-framework}/contra-tracer"
          "${inputs.iohk-monitoring-framework}/iohk-monitoring"
          "${inputs.iohk-monitoring-framework}/tracer-transformers"
          "${inputs.iohk-monitoring-framework}/plugins/backend-ekg"
          "${inputs.iohk-monitoring-framework}/plugins/backend-aggregation"
          "${inputs.iohk-monitoring-framework}/plugins/backend-monitoring"
          "${inputs.iohk-monitoring-framework}/plugins/backend-trace-forwarder"

          "${inputs.cardano-node}/cardano-api"
          "${inputs.cardano-node}/cardano-cli"
          "${inputs.cardano-node}/cardano-git-rev"
          "${inputs.cardano-node}/cardano-node"
          "${inputs.cardano-node}/trace-dispatcher"
          "${inputs.cardano-node}/trace-forward"
          "${inputs.cardano-node}/trace-resources"

          "${inputs.goblins}"

          "${inputs.io-sim}/io-classes"
          "${inputs.io-sim}/io-sim"
          "${inputs.io-sim}/strict-stm"

          "${inputs.typed-protocols}/typed-protocols"
          "${inputs.typed-protocols}/typed-protocols-cborg"
          "${inputs.typed-protocols}/typed-protocols-examples"

          "${inputs.ekg-forward}"

          "${inputs.ekg-json}"

          "${inputs.optparse-applicative}"

          "${inputs.freer-extras}"

          "${inputs.flat}"

          "${inputs.freer-extras}"

          "${inputs.hw-aeson}"
        ];

      cabalProjectLocal = ''
        allow-newer: *:aeson, *:hedgehog, size-based:template-haskell, *:hw-aeson
        constraints: aeson >= 2, hedgehog >= 1.1
      '';

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plainPkgs = plainNixpkgsFor system;
          hackages = hackagesFor system;

          # hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

          moduleFixes = [
            ({ pkgs, ... }:
              {
                packages = {
                  cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                  cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                  # Workaround for duplicate modules error
                  moo.patches =
                    [ ({ version }: if version == "1.2" then ./patches/0001-removed-duplicate-package.patch else null) ];
                };
              }
            )
          ];

        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          index-state = "2022-05-18T00:00:00Z";
          inherit compiler-nix-name cabalProjectLocal;
          inherit (hackages) extra-hackages extra-hackage-tarballs;

          # Workaround for bug in PSM at the commit we need
          # Can't bump to newer commit in PSM with the fix as this leads
          # to incomapible compiler version issues
          modules = [
            ({ config, ... }: {
              packages.plutus-simple-model.doHaddock = false;
            })
          ] ++ moduleFixes ++ hackages.modules;

          shell = {
            inherit (preCommitCheckFor system) shellHook;
            # withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              plainPkgs.cabal-install
              # plainPkgs.fd
              # plainPkgs.haskellPackages.apply-refact
              # plainPkgs.haskellPackages.cabal-fmt
              # plainPkgs.nixpkgs-fmt

              # (fourmoluFor system)
              # (hlintFor system)
              # hls
            ];
          };
        };
    in
    {
      inherit plainNixpkgsFor hackagesFor cabalProjectLocal;

      project = perSystem projectFor;
      flake = perSystem (system: self.project.${system}.flake { });

      packages = perSystem (system:
        self.flake.${system}.packages
      );

      devShells = perSystem (system: {
        default = self.flake.${system}.devShell;

        tooling =
          let
            pkgs = plainNixpkgsFor system;
          in
          pkgs.mkShell {
            inherit (preCommitCheckFor system) shellHook;

            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.fd
              pkgs.haskellPackages.apply-refact
              # pkgs.haskellPackages.cabal-fmt
              # pkgs.nixpkgs-fmt
              # (hlintFor system)
              # (fourmoluFor system)
            ];
          };
      });

      # checks = perSystem (system:
      #   self.flake.${system}.checks
      #   // { formatCheck = preCommitCheckFor system; }
      # );

      # hydraJobs = {
      #   inherit (self) checks packages devShells;
      # };
    };
}
