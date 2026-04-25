{ mkDerivation, attoparsec, base, blaze-html, bytestring
, classy-prelude, hspec, http-types, ihp, lib, mtl
, string-conversions, tasty-bench, text, unordered-containers
, uri-encode, uuid, wai, wai-extra
}:
mkDerivation {
  pname = "ihp-autoroute";
  version = "1.5.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base blaze-html bytestring classy-prelude http-types ihp
    mtl string-conversions text unordered-containers uri-encode uuid
    wai
  ];
  testHaskellDepends = [
    attoparsec base bytestring classy-prelude hspec http-types ihp
    string-conversions text uuid wai wai-extra
  ];
  benchmarkHaskellDepends = [
    base bytestring http-types ihp tasty-bench wai
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Legacy AutoRoute typeclass for IHP";
  license = lib.licenses.mit;
}
