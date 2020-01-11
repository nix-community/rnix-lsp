{ pkgs_fn ? import <nixpkgs> }:

let
  overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = pkgs_fn { overlays = [ overlay ]; };
  nightly = nixpkgs.latest.rustChannels.stable;
in
nixpkgs.mkShell {
  PATH = "${builtins.getEnv "PATH"}:${toString ./target/debug}";
  buildInputs = [ nightly.rust ];
}
