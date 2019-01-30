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
  cargoSha256 = "1n01dzm0ngy1kn42xdmkcc83cxkk9552l5spd5a4ack4s4rdlrm2";

  # See https://github.com/NixOS/nixpkgs/issues/25863#issuecomment-302633494
  RUSTFLAGS="-L ${nightly.rust}/lib/rustlib/x86_64-unknown-linux-gnu/lib/";
}
