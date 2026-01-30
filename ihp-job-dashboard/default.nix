{ mkDerivation, base, blaze-html, blaze-markup, http-types, ihp
, ihp-hsx, lib, mtl, postgresql-simple, wai, wai-request-params
}:
mkDerivation {
  pname = "ihp-job-dashboard";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html blaze-markup http-types ihp ihp-hsx mtl
    postgresql-simple wai wai-request-params
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Dashboard for IHP job runners";
  license = lib.licenses.mit;
}
