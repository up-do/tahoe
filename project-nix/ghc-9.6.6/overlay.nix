  self: super: {
    ghcVersion = "ghc966";

    all-cabal-hashes =
        # Update revision to match required hackage
        super.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e5ab1dca27a34bbec5920863c18d8cd06b0c288d.tar.gz";
          sha256 = "sha256-qJt8ZAG/Ilx0JMyd/FTuHB7k5SpYqkLJFUCi3uEBhvU=";
    };

    # haskellPackages = super.haskell.packages.${self.ghcVersion}.override {
    #   overrides = haskellSelf: haskellSuper: {
    #     crypton = haskellSuper.callHackage "crypton" "1.0.0" {};
    #     fec = haskellSuper.callHackage "fec" "0.2.0" {};
    #     network-simple-tls = haskellSuper.callHackage "network-simple-tls" "0.4.2" {};
    #     servant-js = haskellSuper.callHackage "servant-js" "0.9.4.2" {};
    #     stm-lifted = haskellSuper.callHackage "stm-lifted" "2.5.0.0" {};
    # };

    haskellPackages = super.haskellPackages.override (old: {
      overrides =
        let
          nlib = self.lib;
          hlib = self.haskell.lib;

          spkgs =
              hlib.packageSourceOverrides {
                tahoe-s3 = ./../../s3;
                tahoe-capabilities = ./../../capabilities;
                tahoe-great-black-swamp-types = ./../../swamp/types;
              };

          dpkgs = 
              hlib.packagesFromDirectory {
                directory = ./packages;
              };

          wpkgs = x: _:
            {
              tahoe-great-black-swamp = x.callCabal2nixWithOptions "tahoe-great-black-swamp" ./../../swamp/swamp "--no-check" {};
              tahoe-great-black-swamp-testing = x.callCabal2nix "tahoe-great-black-swamp-testing" ./../../swamp/testing {};
            };
        in
          # nlib.fold nlib.composeExtensions (old.overrides or (_: _: { })) [ spkgs dpkgs ];
          # nlib.composeManyExtensions (old.overrides or (_: _: { })) [ spkgs dpkgs ];
          nlib.composeManyExtensions [ spkgs dpkgs wpkgs ];
    });
  }
