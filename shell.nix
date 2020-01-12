{
  sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs {},
  nur ? pkgs.callPackage sources.NUR {},
}:

let
  rustVersion = nur.repos.mozilla.latest.rustChannels.stable;
in pkgs.mkShell {
  PATH = "${builtins.getEnv "PATH"}:${toString ./target/debug}";
  buildInputs = [ rustVersion.rust ];
}
