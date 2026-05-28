{ mkDerivation, base, fast-logger, hasql, hspec, http-types, ihp, ihp-ide
, lib, process, text, uuid, vault, wai, wai-request-params
}:
mkDerivation {
  pname = "ihp-hspec";
  version = "1.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base fast-logger hasql hspec http-types ihp ihp-ide process text uuid
    vault wai wai-request-params
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Test helpers for IHP apps";
  license = lib.licenses.mit;
}
