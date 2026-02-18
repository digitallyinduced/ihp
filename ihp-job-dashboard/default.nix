{ mkDerivation, base, blaze-html, blaze-markup, hasql
, hasql-dynamic-statements, hasql-implicits, hasql-pool, http-types
, ihp, ihp-auto-refresh, ihp-hsx, lib, mtl, text, wai
, wai-request-params
}:
mkDerivation {
  pname = "ihp-job-dashboard";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-html blaze-markup hasql hasql-dynamic-statements
    hasql-implicits hasql-pool http-types ihp ihp-auto-refresh ihp-hsx
    mtl text wai wai-request-params
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Dashboard for IHP job runners";
  license = lib.licenses.mit;
}
