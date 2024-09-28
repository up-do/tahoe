{
  description = "tahoe-great-black-swamp-types";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://gitlab.com/tahoe-lafs/hs-flake-utils.git?ref=main";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
    ghcVersion = "ghc94";
  in
    ulib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system: let
      # Get a nixpkgs customized for this system and including our overlay.
      pkgs = import nixpkgs {
        inherit system;
      };
      hslib = hs-flake-utils.lib {
        inherit pkgs;
        src = ./.;
        compilerVersion = ghcVersion;
        packageName = "tahoe-great-black-swamp-types";
      };

      # string -> flake app
      #
      # make a flake app that runs the test suite using cabal and the given
      # version of ghc.  The ghc version is found in the nixpkgs
      # `haskell.compiler` attribute set.
      mkCabalTest = ghcVersion: {
        type = "app";
        program = "${
          pkgs.writeShellApplication {
            name = "cabal-build-and-test";
            runtimeInputs = with pkgs; [
              pkg-config
              gcc
              haskell.compiler.${ghcVersion}
              cabal-install
            ];

            text = ''
              set -ex
              cabal update hackage.haskell.org
              cabal build --enable-tests
              cabal run --enable-tests tests
            '';
          }
        }/bin/cabal-build-and-test";
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {
        extraBuildInputs = pkgs: with pkgs; [zlib pkg-config gcc];
        shellHook = ''
          nix run .#write-cabal-project
        '';
      };
      packages = hslib.packages {};
      apps.hlint = hslib.apps.hlint {};

      # Using the working directory of `nix run`, do a build with cabal and
      # then run the test suite.
      apps.cabal-test-8107 = mkCabalTest "ghc8107";
      apps.cabal-test-902 = mkCabalTest "ghc902";
      apps.cabal-test-924 = mkCabalTest "ghc924";
      apps.cabal-test-943 = mkCabalTest "ghc943";

      apps.write-cabal-project = hslib.apps.write-cabal-project {
        localPackages = {
        };
      };

      apps.cabal-test = self.outputs.apps.${system}.cabal-test-943;

      apps.release = {
        type = "app";
        program = "${
          pkgs.writeShellApplication {
            name = "release";
            runtimeInputs = with pkgs; [
              cabal-install
              haskell.compiler.${ghcVersion}
            ];
            text = ''
              set -x
              sdist=$(cabal sdist | tail -n 1)
              haddocks=$(cabal haddock --enable-doc --haddock-for-hackage | tail -n 1)
              cabal upload "$sdist"
              cabal upload --documentation "$haddocks"
            '';
          }
        }/bin/release";
      };
    });
}
