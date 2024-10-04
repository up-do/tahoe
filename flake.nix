# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0
{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      # pkgs = nixpkgs.legacyPackages.${system};
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import ./project-nix/ghc-9.6.6/overlay.nix)
        ];
      };

      #haskellPackages = pkgs.haskell.packages.ghc963;
      haskellPackages = pkgs.haskellPackages;

      jailbreakUnbreak = pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      packageName = "tahoe-s3";
    in {
      packages.${packageName} = pkgs.haskellPackages.${packageName};

      packages.default = self.packages.${system}.${packageName};

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cabal-install
        ];
        inputsFrom = [self.packages.${system}.${packageName}.env];
        shellHook = "PS1=\"[tahoe-s3:\\w]$ \"";
      };
    });
}