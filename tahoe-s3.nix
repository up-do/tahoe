{ mkDerivation, aeson, amazonka, amazonka-s3, async, base
, base32string, base64-bytestring, bytestring, composition, conduit
, containers, directory, exceptions, fingertree, hashable, hspec
, hspec-expectations, http-client, http-types, lens, lib, memory
, mtl, QuickCheck, quickcheck-classes, quickcheck-instances, stm
, stm-delay, stm-lifted, tahoe-great-black-swamp
, tahoe-great-black-swamp-testing, tahoe-great-black-swamp-types
, tasty, temporary, text, ttrie, unordered-containers
}:
mkDerivation {
  pname = "tahoe-s3";
  version = "0.1.0.0";
  src = ./.;
  postUnpack = "sourceRoot+=/s3; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-s3 async base base64-bytestring bytestring
    composition conduit containers exceptions fingertree hashable
    http-client http-types lens memory mtl stm-delay stm-lifted
    tahoe-great-black-swamp tahoe-great-black-swamp-types text ttrie
    unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    amazonka amazonka-s3 base base32string bytestring composition
    containers directory fingertree hspec hspec-expectations http-types
    lens QuickCheck quickcheck-classes quickcheck-instances stm
    stm-delay stm-lifted tahoe-great-black-swamp
    tahoe-great-black-swamp-testing tahoe-great-black-swamp-types tasty
    temporary text ttrie
  ];
  license = lib.licenses.asl20;
  mainProgram = "tahoe-s3";
}
