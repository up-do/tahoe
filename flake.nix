{
  description = "tahoe-capabilities";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = github:numtide/flake-utils;
    hs-flake-utils.url = "git+https://whetstone.private.storage/jcalderone/hs-flake-utils.git?ref=main";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
  in
    ulib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "ghc8107";
      hslib = hs-flake-utils.lib {
        inherit pkgs;
        src = ./.;
        compilerVersion = ghcVersion;
        packageName = "tahoe-capabilities";
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {};
      packages = hslib.packages {};

      apps.hlint = hslib.apps.hlint {};

      # Using the working directory of `nix run`, do a build with cabal and
      # then run the test suite.
      apps.cabal-test = {
        type = "app";
        program = "${
          pkgs.writeShellApplication {
            name = "cabal-build-and-test";
            runtimeInputs = with pkgs; [pkg-config haskell.compiler.${ghcVersion} cabal-install];

            text = ''
              set -ex
              cabal update hackage.haskell.org
              cabal build --enable-tests
              runtests=$(cabal list-bin --enable-tests tahoe-capabilities-test)
              eval "$runtests"
            '';
          }
        }/bin/cabal-build-and-test";
      };
      apps.release = {
        type = "app";
        program = "${
          pkgs.writeShellApplication {
            name = "release";
            runtimeInputs = with pkgs; [cabal-install];
            text = ''
              set -x
              sdist=$(cabal sdist | tail -n 1)
              haddocks=$(cabal haddock --haddock-for-hackage | tail -n 1)
              cabal upload "$sdist"
              cabal upload --documentation "$haddocks"
            '';
          }
        }/bin/release";
      };
    });
}
