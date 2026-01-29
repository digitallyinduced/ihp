{ mkDerivation, aeson, async, attoparsec, base, bytestring
, case-insensitive, classy-prelude, containers, deepseq
, haskell-src-exts, haskell-src-meta, hspec, http-media, http-types
, ihp, ihp-hsx, ihp-log, ihp-postgresql-simple-extra, interpolate
, lib, mono-traversable, mtl, postgresql-simple, resource-pool
, safe-exceptions, scientific, stm, template-haskell, text, time
, transformers, unliftio, unordered-containers, uuid, vector, wai
, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "ihp-datasync";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring case-insensitive
    classy-prelude containers deepseq haskell-src-exts haskell-src-meta
    http-media http-types ihp ihp-hsx ihp-log
    ihp-postgresql-simple-extra interpolate mono-traversable mtl
    postgresql-simple resource-pool safe-exceptions scientific stm
    template-haskell text time transformers unliftio
    unordered-containers uuid vector wai wai-websockets warp websockets
  ];
  testHaskellDepends = [
    aeson async attoparsec base bytestring case-insensitive
    classy-prelude containers deepseq haskell-src-exts haskell-src-meta
    hspec http-media http-types ihp ihp-hsx ihp-log
    ihp-postgresql-simple-extra interpolate mono-traversable mtl
    postgresql-simple resource-pool safe-exceptions scientific stm
    template-haskell text time transformers unliftio
    unordered-containers uuid vector wai wai-websockets warp websockets
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "IHP DataSync Framework";
  license = lib.licenses.mit;
}
