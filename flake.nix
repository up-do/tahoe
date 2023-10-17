{
  description = "tahoe-s3";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://whetstone.private.storage/jcalderone/hs-flake-utils.git?ref=main";
    tahoe-great-black-swamp.url = "git+https://whetstone.private.storage/privatestorage/tahoe-great-black-swamp.git?ref=release-0.4.0.0";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    tahoe-great-black-swamp,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
    ghcVersion = "ghc8107";
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
        packageName = "tahoe-s3";
        hsPkgsOverrides = hfinal: hprev: {
          tahoe-great-black-swamp = tahoe-great-black-swamp.outputs.packages.${system}.default;
          crypton = hfinal.callHackageDirect {
            pkg = "crypton";
            ver = "0.33";
            sha256 = "sha256-bq1ypwOhYC8OR5XDWDj0u4+gTygxcwnPL+IffUWvlhg=";
          } {};
          amazonka-core = hfinal.callHackageDirect {
            pkg = "amazonka-core";
            ver = "2.0";
            sha256 = "sha256-KVTe6IlVDNaot1XuFjbvlUs/jmeoyEfqnDYsb4V1K2g=";
          } {};
          amazonka-sso = hfinal.callHackageDirect {
            pkg = "amazonka-sso";
            ver = "2.0";
            sha256 = "sha256-+632/wu9Vdii8f5NwFeypXUeUV5b3DgMonUluiwO3F0=";
          } {};
          amazonka-sts = hfinal.callHackageDirect {
            pkg = "amazonka-sts";
            ver = "2.0";
            sha256 = "sha256-5eRQ5zH5gsoiJZMwq4eepUyDBHzrIZFOPA6vKOCSuHQ=";
          } {};
          amazonka-s3 = hfinal.callHackageDirect {
            pkg = "amazonka-s3";
            ver = "2.0";
            sha256 = "sha256-NHARJB4pGQFNUgoPSAdcibrXVD2nS+vqLxcP0/4bA7I=";
          } {};
          amazonka-test = hfinal.callHackageDirect {
            pkg = "amazonka-test";
            ver = "2.0";
            sha256 = "sha256-lFXvtzj4p3aqMpRMyCz32jpkET3tE7BaUf6+2iwQ/ok=";
          } {};
          amazonka = hfinal.callHackageDirect {
            pkg = "amazonka";
            ver = "2.0";
            sha256 = "sha256-ut71byLgmg9SgZCfIrDbG76LI7Buq+x6F4oHTTuEdHI=";
          } {};
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
          tahoe-great-black-swamp = tahoe-great-black-swamp.sourceInfo.outPath;
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
