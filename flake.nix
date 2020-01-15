{
  description = "A work-in-progress language server for Nix, with syntax checking and basic completion";

  edition = 201909;

  inputs.naersk = {
    url   = "github:nmattia/naersk";
    flake = false;
  };
  inputs.mozilla = {
    url   = "github:mozilla/nixpkgs-mozilla";
    flake = false;
  };
  inputs.jd91mzm2 = {
    url = "git+https://gitlab.com/jD91mZM2/nur-packages";
  };

  outputs = { self, nixpkgs, mozilla, naersk, jd91mzm2 }: let
    forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "i686-linux" "aarch64-linux" ];
  in
    rec {
      # Main package
      packages = forAllSystems (system: {
        rnix-lsp = let
          mozillaBuilt = nixpkgs.legacyPackages."${system}".callPackage "${mozilla}/package-set.nix" {};
          naerskBuilt  = nixpkgs.legacyPackages."${system}".callPackage naersk {};

          rust = mozillaBuilt.latest.rustChannels.stable.rust;
        in naerskBuilt.buildPackage {
          name = "rnix-lsp";
          src  = jd91mzm2.lib.cleanSourceRust ./.;
          root = ./.;

          cargo = rust.cargo;
          rustc = rust.rustc;
        };
      });
      defaultPackage = forAllSystems (system: packages."${system}".rnix-lsp);

      # Make it runnable with `nix app`
      apps = forAllSystems (system: {
        rnix-lsp = {
          type    = "app";
          program = "${self.packages."${system}".rnix-lsp}/bin/rnix-lsp";
        };
      });
      defaultApp = forAllSystems (system: apps."${system}".rnix-lsp);
    };
}
