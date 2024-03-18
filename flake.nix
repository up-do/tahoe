{
  description = "tahoe-lafs server in Haskell";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://gitlab.com/tahoe-lafs/hs-flake-utils.git?ref=main";

    tahoe-chk = {
      url = "git+https://gitlab.com/tahoe-lafs/tahoe-chk.git?ref=main";
      inputs.nixpkgs.follows = "hs-flake-utils/nixpkgs";
      inputs.hs-flake-utils.follows = "hs-flake-utils";
    };
    tahoe-great-black-swamp-types = {
      url = "git+https://gitlab.com/tahoe-lafs/tahoe-great-black-swamp-types?ref=main";
      inputs.nixpkgs.follows = "hs-flake-utils/nixpkgs";
      inputs.hs-flake-utils.follows = "hs-flake-utils";
    };
    tahoe-great-black-swamp-testing = {
      url = "git+https://gitlab.com/tahoe-lafs/tahoe-great-black-swamp-testing?ref=main";
      inputs.nixpkgs.follows = "hs-flake-utils/nixpkgs";
      inputs.hs-flake-utils.follows = "hs-flake-utils";
      inputs.tahoe-great-black-swamp-types.follows = "tahoe-great-black-swamp-types";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
    tahoe-chk,
    tahoe-great-black-swamp-types,
    tahoe-great-black-swamp-testing,
  }: let
    ulib = flake-utils.lib;
    ghcVersion = "ghc8107";
  in
    ulib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system: let
      # Get a nixpkgs customized for this system
      pkgs = import nixpkgs {
        inherit system;

        # language-ecmascript is marked as broken but it isn't broken, let it
        # through.  Remove this as soon as we can mark it more specifically.
        # It would be nice if we could give hs-flake-utils a package override
        # to use.
        config = {
          allowBroken = true;
        };
      };
      hslib = hs-flake-utils.lib {
        inherit pkgs;
        src = ./.;
        compilerVersion = ghcVersion;
        packageName = "haskell-tahoe-lafs-storage-server";
        hsPkgsOverrides = hfinal: hprev: {
          tahoe-chk = tahoe-chk.outputs.packages.${system}.default;
          tahoe-great-black-swamp-types = tahoe-great-black-swamp-types.outputs.packages.${system}.default;
          tahoe-great-black-swamp-testing = tahoe-great-black-swamp-testing.outputs.packages.${system}.default;
        };
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {
        extraBuildInputs = pkgs:
          with pkgs; [
            # We configure the zlib package to use pkg-config to find the C
            # library pieces, so make sure pkg-config is around.
            pkg-config
            # And the C library pieces, too.
            zlib.dev
          ];
      };
      packages = hslib.packages {};
      apps = {
        write-cabal-project = hslib.apps.write-cabal-project {
          localPackages = {
            tahoe-chk = tahoe-chk.sourceInfo.outPath;
            tahoe-great-black-swamp-types = tahoe-great-black-swamp-types.sourceInfo.outPath;
            tahoe-great-black-swamp-testing = tahoe-great-black-swamp-testing.sourceInfo.outPath;
          };
        };
        cabal-test = hslib.apps.cabal-test {
          preBuild = ''
            # Refresh the cabal.project.local file to point to the correct
            # dependencies, if necessary.
            nix run .#write-cabal-project

            # Also, zlib doesn't want to use pkg-config by default.  Convince
            # it...
            cat >>cabal.project.local <<EOF
            package zlib
                flags: +pkg-config
            EOF

            # Here we make zlib discoverable by pkg-config so cabal can find
            # headers and stuff.
            export PKG_CONFIG_PATH=${pkgs.lib.makeSearchPath "lib/pkgconfig" [pkgs.zlib.dev]}
          '';
        };
        release = hslib.apps.release {};
        hlint = hslib.apps.hlint {};
      };
    });
}
