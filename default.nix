let
  overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ overlay ]; };
  nightly = nixpkgs.latest.rustChannels.nightly;
  buildRustPackage = nixpkgs.rustPlatform.buildRustPackage.override {
    rust = nightly;
  };
in buildRustPackage {
  name = "nix-lsp";
  src = builtins.filterSource (path: _type: path != (toString ./target)) ./.;
  cargoSha256 = "0fvybsg3l88rag45b8dg3fvr3cisgjjvs4ma6sr60nh05202aqhn";

  # See https://github.com/NixOS/nixpkgs/issues/25863#issuecomment-302633494
  RUSTFLAGS="-L ${nightly.rust}/lib/rustlib/x86_64-unknown-linux-gnu/lib/";
}
