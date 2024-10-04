{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lib, stm, transformers }:
      mkDerivation {
        pname = "stm-lifted";
        version = "2.5.0.0";
        sha256 = "a818313be5acbf089b0ea6b4b76d49b70f16fcda58b647a0588f2124f4804a7f";
        libraryHaskellDepends = [ base stm transformers ];
        jailbreak = true;
        description = "Software Transactional Memory lifted to MonadIO";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
