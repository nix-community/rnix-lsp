let
  overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ overlay ]; };
  nightly = nixpkgs.rustChannelOf {
    date = "2019-04-03";
    channel = "nightly";
  };
  buildRustPackage = nixpkgs.rustPlatform.buildRustPackage.override {
    rust = {
      rustc = nightly.rust;
      cargo = nightly.cargo;
    };
  };
in buildRustPackage {
  name = "nix-lsp";
  src = builtins.filterSource (path: _type: path != (toString ./target)) ./.;
  cargoSha256 = "08zx7jmlnq27ksw3l7ww6qbmgli8fwv96fy5ymdgzw2bly951hnd";
}
