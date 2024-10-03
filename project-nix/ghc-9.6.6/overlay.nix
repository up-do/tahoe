  self: super: {
    ghcVersion = "ghc963";

    all-cabal-hashes =
        # Update revision to match required hackage
        super.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e5ab1dca27a34bbec5920863c18d8cd06b0c288d.tar.gz";
          sha256 = "sha256-qJt8ZAG/Ilx0JMyd/FTuHB7k5SpYqkLJFUCi3uEBhvU=";
    };

    haskellPackages = super.haskell.packages.${self.ghcVersion}.override {
      overrides = haskellSelf: haskellSuper: {
        crypton = haskellSuper.callHackage "crypton" "1.0.0" {};
        fec = haskellSuper.callHackage "fec" "0.2.0" {};
        network-simple-tls = haskellSuper.callHackage "network-simple-tls" "0.4.2" {};
        servant-js = haskellSuper.callHackage "servant-js" "0.9.4.2" {};
        stm-lifted = haskellSuper.callHackage "stm-lifted" "2.5.0.0" {};
      };
    };
  }
