{ pkgs_fn ? import <nixpkgs> }:

let
  overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = pkgs_fn { overlays = [ overlay ]; };
  nightly = nixpkgs.rustChannelOf {
    date = "2019-08-25";
    channel = "nightly";
  };
in
nixpkgs.mkShell {
  buildInputs = [ nightly.rust ];
}
