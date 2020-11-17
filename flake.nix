{
  description = "A work-in-progress language server for Nix, with syntax checking and basic completion";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.naersk.url = "github:nmattia/naersk";
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils, naersk }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        naersk-lib = pkgs.callPackage naersk { };
      in
      {

        defaultPackage = naersk-lib.buildPackage ./.;

        defaultApp = {
          type = "app";
          program = "${self.defaultPackage."${system}"}/bin/rnix-lsp";
        };
      });
}
