{ inputs.all-cabal-hashes = {
    url = "github:commercialhaskell/all-cabal-hashes/hackage";

    flake = false;
  };

  outputs = { all-cabal-hashes, flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config.allowUnfree = true;

        pkgs = import nixpkgs {
          inherit config system;

          overlays = [ self.overlays.default ];
        };

      in
        { packages.default = pkgs.haskellPackagesCustom.haskell-examples;

          devShells.default = pkgs.haskellPackagesCustom.shellFor {
            packages = hpkgs: [
              hpkgs.haskell-examples
            ];
            
            # This is used by `nix develop .` to open a devShell
      devShells = let
        shell = import ./shell.nix {inherit pkgs customConfig;};
      in {
        inherit (shell) devops workbench-shell;
        default = shell.dev;
        cluster = shell;
        profiled = project.profiled.shell;
      };

            nativeBuildInputs = [
              pkgs.haskell-language-server

              (pkgs.vscode-with-extensions.override {
                vscodeExtensions = [
                  pkgs.vscode-extensions.haskell.haskell
                  pkgs.vscode-extensions.justusadam.language-haskell
                ];
              })
            ];

            withHoogle = true;

            doBenchmark = true;
          };
        }
    ) // {
      overlays.default = self: super: {
        inherit all-cabal-hashes;

        haskellPackagesCustom = self.haskellPackages.override (old: {
          overrides =
            let
              hlib = self.haskell.lib.compose;
            in
              self.lib.composeManyExtensions [
                (hlib.packageSourceOverrides {
                  haskell-examples = ./.;
                })

                (hlib.packagesFromDirectory {
                  directory = ./dependencies;
                })

                (hself: hsuper: {
                })
              ];
        });
      };
    };
}
