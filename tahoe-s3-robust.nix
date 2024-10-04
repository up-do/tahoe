let
  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          nlib = pkgsNew.lib;
          hlib = pkgsNew.haskell.lib;

          spkgs =
              hlib.packageSourceOverrides {
                tahoe-s3 = ./s3;
                tahoe-capabilities = ./capabilities;
                tahoe-great-black-swamp-types = ./swamp/types;
              };

          dpkgs = 
              hlib.packagesFromDirectory {
                directory = ./project-nix/ghc-9.6.6/packages;
              };

          wpkgs = x: _:
            {
              tahoe-great-black-swamp = x.callCabal2nixWithOptions "tahoe-great-black-swamp" ./swamp/swamp "--no-check" {};
              tahoe-great-black-swamp-testing = x.callCabal2nix "tahoe-great-black-swamp-testing" ./swamp/testing {};
            };
        in
          # nlib.fold nlib.composeExtensions (old.overrides or (_: _: { })) [ spkgs dpkgs ];
          # nlib.composeManyExtensions (old.overrides or (_: _: { })) [ spkgs dpkgs ];
          nlib.composeManyExtensions [ spkgs dpkgs wpkgs ];
    });
  };

  pkgs = import <nixpkgs> { overlays = [ overlay ]; };

in
  pkgs.haskellPackages.tahoe-s3.env
