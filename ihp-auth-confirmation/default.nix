{ mkDerivation, base, ihp, ihp-mail, lib, text, wai }:
mkDerivation {
  pname = "ihp-auth-confirmation";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [ base ihp ihp-mail text wai ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Email confirmation support for IHP authentication";
  license = lib.licenses.mit;
}
