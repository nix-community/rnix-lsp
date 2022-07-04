{
  description = "A work-in-progress language server for Nix, with syntax checking and basic completion";

  inputs = {
    naersk.url = "github:nix-community/naersk";
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, utils, naersk }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        naersk-lib = naersk.lib."${system}";
      in
      rec {
        packages.rnix-lsp = naersk-lib.buildPackage {
          pname = "rnix-lsp";
          root = ./.;
          doCheck = true;
          checkInputs = [ pkgs.nix_2_4 ];
        };
        defaultPackage = packages.rnix-lsp;

        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            rustc
            cargo
            gitAndTools.pre-commit
          ];
        };

        apps.rnix-lsp = utils.lib.mkApp {
          drv = packages.rnix-lsp;
        };
        defaultApp = apps.rnix-lsp;
      });
}
