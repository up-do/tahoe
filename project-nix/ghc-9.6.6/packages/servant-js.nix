{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base-compat, charset, hspec
      , hspec-discover, hspec-expectations, language-ecmascript, lens
      , lib, QuickCheck, servant, servant-foreign, text
      }:
      mkDerivation {
        pname = "servant-js";
        version = "0.9.4.2";
        sha256 = "bea36aa98c8f13c3b42794006d083ee4ab1b2ff448717064979d8544e2d0c596";
        revision = "5";
        editedCabalFile = "05iwi5q2hbaqc7n1zhw9zpj4qcw8mg849zjfxfv84c9wwh35nrxa";
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base-compat charset lens servant servant-foreign text
        ];
        testHaskellDepends = [
          base base-compat hspec hspec-expectations language-ecmascript lens
          QuickCheck servant text
        ];
        testToolDepends = [ hspec-discover ];
        doCheck = false;
        homepage = "http://haskell-servant.readthedocs.org/";
        description = "Automatically derive javascript functions to query servant webservices";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
