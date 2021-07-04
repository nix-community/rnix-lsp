{
  description = "A work-in-progress language server for Nix, with syntax checking and basic completion";

  inputs = {
    naersk.url = "github:nmattia/naersk";
    utils.url = "github:numtide/flake-utils";
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
