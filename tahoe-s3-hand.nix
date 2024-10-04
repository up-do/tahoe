let
  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides = pkgsNew.haskell.lib.packageSourceOverrides {
        tahoe-s3 = ./s3;

        crypton = "1.0.0";
        fec = "0.2.0";
        network-simple-tls = "0.4.2";
        servant-js = "0.9.4.2";
        stm-lifted = "2.5.0.0";
      };
    });
  };

  pkgs = import <nixpkgs> { overlays = [ overlay ]; };

in
  pkgs.haskellPackages.tahoe-s3.env
