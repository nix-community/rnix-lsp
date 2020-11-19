{
  sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs {},
  naersk ? pkgs.callPackage sources.naersk {},
}:
naersk.buildPackage ./.
