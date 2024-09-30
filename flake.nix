{
  description = "tahoe-s3";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://gitlab.com/tahoe-lafs/hs-flake-utils.git?ref=main";
    tahoe-great-black-swamp-testing.url = "path:swamp/testing";
    tahoe-great-black-swamp-types.url = "path:swamp/types";
    tahoe-great-black-swamp.url = "path:swamp/swamp";
    tahoe-capabilities.url = "path:capabilities";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    tahoe-great-black-swamp,
    tahoe-great-black-swamp-types,
    tahoe-great-black-swamp-testing,
    tahoe-capabilities,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
    ghcVersion = "ghc810";
  in
    ulib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system: let
      # Get a nixpkgs customized for this system and including our overlay.
      pkgs = import nixpkgs {inherit system;};
      hslib = hs-flake-utils.lib {
        inherit pkgs;
        src = ./s3;
        compilerVersion = ghcVersion;
        packageName = "tahoe-s3";
        hsPkgsOverrides = hfinal: hprev: {
          tahoe-capabilities =
            tahoe-capabilities.outputs.packages.${system}.default;
          tahoe-great-black-swamp =
            tahoe-great-black-swamp.outputs.packages.${system}.default;
          tahoe-great-black-swamp-types =
            tahoe-great-black-swamp-types.outputs.packages.${system}.default;
          tahoe-great-black-swamp-testing =
            tahoe-great-black-swamp-testing.outputs.packages.${system}.default;
          transformers = hfinal.callHackage "transformers" "0.5.6.2" {};
          OneTuple = hfinal.callHackageDirect {
            pkg = "OneTuple";
            ver = "0.4.2";
            sha256 = "sha256-r/Vag2d5pJSb2xQIlNebJYBQ2QIVt8jpdiXDnFptgKQ=";
           } {};
          aeson = hfinal.callHackage "aeson" "1.5.6.0" {};
          amazonka = hfinal.callHackage "amazonka" "2.0" {};
          amazonka-core = hfinal.callHackage "amazonka-core" "2.0" {};
          amazonka-s3 = hfinal.callHackage "amazonka-s3" "2.0" {};
          amazonka-sso = hfinal.callHackage "amazonka-sso" "2.0" {};
          amazonka-sts = hfinal.callHackage "amazonka-sts" "2.0" {};
          crypton = hfinal.callHackage "crypton" "1.0.0" {};
          data-array-byte = hfinal.callHackage "data-array-byte" "0.1.0.1" {};
          fec = hfinal.callHackage "fec" "0.2.0" {};
          foldable1-classes-compat = hfinal.callHackage "foldable1-classes-compat" "0.1" {};
          hashable = hfinal.callHackageDirect {
            pkg = "hashable";
            ver = "1.4.7.0";
            sha256 = "sha256-ykQ0pQ2SIUPnkESkuBNBRT7kwtl7pLYKJ+s0OYei0/0=";
           } {};
          http-api-data = hfinal.callHackage "http-api-data" "0.4.3" {};
          lens = hfinal.callHackage "lens" "5.2.3" {};
          network = hfinal.callHackage "network" "3.1.4.0" {};
          quickcheck-instances = hfinal.callHackage "quickcheck-instances" "0.3.30" {};
          semialign = hfinal.callHackage "semialign" "1.2.0.1" {};
          servant-docs = hfinal.callHackage "servant-docs" "0.12" {};
          servant-foreign = hfinal.callHackage "servant-foreign" "0.15.4" {};
          servant-js = hfinal.callHackage "servant-js" "0.9.4.2" {};
          tahoe-chk = hfinal.callHackage "tahoe-chk" "0.2.0.0" {};
          text-short = hfinal.callHackage "text-short" "0.1.6" {};
          tree-diff = hfinal.callHackage "tree-diff" "0.3.0.1" {};
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
              zlib
              pkg-config
              gcc
              haskell.compiler.${ghcVersion}
              cabal-install
            ];

            text = ''
              set -ex
              cabal update hackage.haskell.org
              cabal build all --enable-tests
              cabal run tahoe-s3 --enable-tests tests
            '';
          }
        }/bin/cabal-build-and-test";
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {
        extraBuildInputs = pkgs: with pkgs; [zlib pkg-config gcc];
      };
      packages = hslib.packages {};
      apps.hlint = hslib.apps.hlint {};

      # Using the working directory of `nix run`, do a build with cabal and
      # then run the test suite.
      apps.cabal-test-8107 = mkCabalTest "ghc8107";
      apps.cabal-test-902 = mkCabalTest "ghc902";
      apps.cabal-test-924 = mkCabalTest "ghc924";
      apps.cabal-test-943 = mkCabalTest "ghc943";

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
