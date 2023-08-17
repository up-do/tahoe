{
  description = "tahoe-lafs server in Haskell";

  inputs = {
    # Nix Inputs
    nixpkgs.follows = "hs-flake-utils/nixpkgs";
    flake-utils.url = github:numtide/flake-utils;
    hs-flake-utils.url = "git+https://whetstone.private.storage/jcalderone/hs-flake-utils.git?ref=main";

    tahoe-chk = {
      url = "git+https://whetstone.private.storage/PrivateStorage/tahoe-chk?ref=refs/tags/0.1.0.0";
      inputs.nixpkgs.follows = "hs-flake-utils/nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
    tahoe-chk,
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
        };
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {
        extraBuildInputs = pkgs:
          with pkgs; [
            zlib
          ];
      };
      packages = hslib.packages {};
      apps = {
        cabal-test = hslib.apps.cabal-test {};
        release = hslib.apps.release {};
        hlint = hslib.apps.hlint {};
      };
    });
}
