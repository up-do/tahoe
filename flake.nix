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
      hslib = hs-flake-utils.lib {
        pkgs = nixpkgs.legacyPackages.${system};
        src = ./.;
        compilerVersion = "ghc8107";
        packageName = "tahoe-capabilities";
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {};
      packages = hslib.packages {};
    });
}
