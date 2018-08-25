let
  overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
in
  with import <nixpkgs> { overlays = [ overlay ]; };
  let
    buildRustPackage = rustPlatform.buildRustPackage.override {
      rust = latest.rustChannels.nightly;
    };
    rnix = fetchFromGitLab {
      owner = "jD91mZM2";
      repo = "rnix";
      rev = "9aeb53e607304481ea887521f2f1505e1e751d2d";

      sha256 = "0nlriv2digy7vxf89pd2r4mfk01gzp06dlhm86hc7g6azcz8yfi3";
    };
    rnixEscaped = builtins.replaceStrings ["/"] ["\\/"] (toString rnix);
  in buildRustPackage {
    name = "nix-lsp";
    src = builtins.filterSource (path: _type: path != (toString ./target)) ./.;
    cargoSha256 = "0l1bmj9lckv81biff8z1nbq5j5n2p4mikbnjn67szkvk4kjw3sg9";
    preBuild = ''
      echo "${rnixEscaped}"
      sed "s/^rnix.*$/rnix = { path = \"${rnixEscaped}\" }/" -i Cargo.toml
      cargo update # update the lock file
    '';
  }
