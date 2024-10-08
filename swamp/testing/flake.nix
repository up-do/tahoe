{
  description = "tahoe-great-black-swamp-testing";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://gitlab.com/tahoe-lafs/hs-flake-utils.git?ref=main";
    tahoe-great-black-swamp-types.url = "git+https://gitlab.com/tahoe-lafs/tahoe-great-black-swamp-types?ref=depfix";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    tahoe-great-black-swamp-types,
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
        packageName = "tahoe-great-black-swamp-testing";
        hsPkgsOverrides = hfinal: hprev: {
          tahoe-great-black-swamp-types = tahoe-great-black-swamp-types.outputs.packages.${system}.default;
        };
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
          tahoe-great-black-swamp-types =
            tahoe-great-black-swamp-types.sourceInfo.outPath;
        };
      };

      apps.cabal-test = self.outputs.apps.${system}.cabal-test-943;

      apps.release = hslib.apps.release {};
    });
}
