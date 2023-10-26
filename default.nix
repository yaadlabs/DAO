# default.nix
# with import <nixpkgs> {};
# with import "github:NixOS/nixpkgs" {};
let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/0ba2543f8c855d7be8e90ef6c8dc89c1617e8a08.tar.gz";
    sha256 = "14ann7vz7qgfrw39ji1s19n1p0likyf2ag8h7rh8iwp3iv5lmprl";
  };
  pkgs = import nixpkgs {};
in pkgs.stdenv.mkDerivation {
    name = "triphut-drv";
    src = "./";
    buildInputs = 
      [ pkgs.pkg-config
        pkgs.zlib
        pkgs.secp256k1
        pkgs.libsodium
        pkgs.systemd
      ];
}
