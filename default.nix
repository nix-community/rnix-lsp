{
  sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs {},
  naersk ? pkgs.callPackage sources.naersk {},
  nur ? pkgs.callPackage sources.NUR {},
}:

let
  rust = nur.repos.mozilla.latest.rustChannels.stable.rust;

  inherit (pkgs) lib;
in naersk.buildPackage {
  name = "rnix-lsp";
  root = nur.repos.jd91mzm2.lib.cleanSourceRust ./.;

  nativeBuildInputs = with pkgs; [ /* pkgconfig */ ];
  buildInputs = with pkgs; [ /* openssl */ ];

  cargo = rust.cargo;
  rustc = rust.rustc;
}
